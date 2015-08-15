(ns pedestal.swagger.core
  (:require [pedestal.swagger.doc :as doc]
            [pedestal.swagger.schema :as schema]
            [pedestal.swagger.body-params :as body-params]
            [schema.core :as s]
            [io.pedestal.http.route.definition :refer [expand-routes]]

            [io.pedestal.interceptor.helpers :as interceptor]
            [io.pedestal.interceptor :refer [interceptor]]
            [ring.util.response :refer [response resource-response redirect]]
            [ring.swagger.swagger2 :as spec]
            [ring.util.http-status :as status]
            [ring.util.http-response :as resp]))

(defn- default-json-converter [swagger-object]
  (spec/swagger-json
   swagger-object
   {:default-response-description-fn
    #(get-in status/status [% :description] "")}))

(defn swagger-json
  "Creates an interceptor that serves the generated documentation on
   the path fo your choice.  Accepts an optional function f that takes
   the swagger-object and returns a ring response."
  ([] (swagger-json default-json-converter))
  ([f]
   (interceptor
    {:name ::doc/swagger-json
     :enter
     (fn [{:keys [route] :as context}]
       (assoc context :response
              {:status 200 :body (f (-> route meta ::doc/swagger-object))}))})))

(defn swagger-ui
  "Creates an interceptor that serves the swagger ui on a path of your
  choice. Note that the path MUST specify a splat argument named
  \"resource\" e.g. \"my-path/*resource\". Acceps additional options
  used to construct the swagger-json url (such as :app-name
  :your-app-name), using pedestal's 'url-for'."
  [& path-opts]
  (interceptor
   {:name ::doc/swagger-ui
    :enter
    (fn [{:keys [request] :as context}]
      (let [{:keys [path-params path-info url-for]} request
            res (:resource path-params)]
        (->> (case res
               "" (redirect (str path-info "index.html"))
               "conf.js" (response (str "window.API_CONF = {url: \""
                                    (apply url-for ::doc/swagger-json path-opts)
                                    "\"};"))
               (resource-response res {:root "swagger-ui/"}))
             (assoc context :response))))}))

(defn coerce-request
  "Creates an interceptor that coerces the incoming request according to the
  selected route swagger documentation. A coercion function f that acceps the
  current request and the route schema and returns a request be supplied. This
  interceptor also catches coercion exceptions and returns unprocessable-entity
  response. You can customise this behaviour assoc'ing your own :error key
  behaviour."
  ([] (coerce-request (schema/make-coerce-request)))
  ([f]
   (doc/annotate
    {:responses {status/unprocessable-entity {}}}
    (interceptor
     {:name ::coerce-request
      :enter
      (fn [{:keys [route request] :as context}]
        (if-let [schema (-> route doc/annotation :parameters)]
          (update context :request (partial f schema))
          context))
      :error
      (fn [context error]
        (def xyz [context error])
        (if (= ::coerce-request (-> error ex-data :interceptor))
          (let [result (-> error ex-data :exception ex-data :error)]
            (assoc context :response
                   (resp/unprocessable-entity (schema/explain result))))
          (throw error)))}))))


(defn validate-response
  "Creates an interceptor that validates the outgoing response according to the
  selected route swagger documentation. A validation function f that acceps the
  current response and the route schema and returns a response be supplied. This
  interceptor also catches coercion exceptions and returns internal-server-error
  response. You can customise this behaviour assoc'ing your own :error key
  behaviour."
  ([] (validate-response (schema/make-validate-response)))
  ([f]
   (doc/annotate
    {:responses {status/internal-server-error {}}}
    (interceptor
     {:name ::validate-response
      :leave
      (fn [{:keys [route response] :as context}]
        (let [extract-schema (fn [responses]
                               (or (responses (:status response))
                                   (responses :default)))]
          (if-let [schema (-> route doc/annotation
                              :responses extract-schema)]
            (update context :response (partial f schema))
            context)))
      :error
      (fn [context error]
        (if (= ::validate-response (-> error ex-data :interceptor))
          (let [result (-> error ex-data :exception ex-data :error)] 
            (assoc context :response
                   (resp/internal-server-error (schema/explain result))))
          (throw error)))}))))

;;;; Pedestal aliases

(defn body-params
  "An almost drop-in replacement for pedestal's body-params.
  Accepts a parser map with content-type strings as keys instead of regexes.
  Ensures the body keys assoc'd into the request are the ones coerce-request
  expects and keywordizes keys by default. Returns a 400 if body-params cannot
  be deserialised. This interceptor also catches deserialization exceptions and
  returns bad-request response. You can customise this behaviour assoc'ing your
  own :error key behaviour."
  ([] (body-params body-params/default-parser-map))
  ([parser-map]
   (doc/annotate
    {:consumes (keys parser-map)
     :responses {status/bad-request {}}}
    (interceptor
     {:name ::body-params
      :enter
      (fn [{:keys [request] :as context}]
        (assoc context :request (body-params/parse-content-type parser-map request)))
      :error
      (fn [context error]
        (if (= ::body-params (-> error ex-data :interceptor))
          (assoc context :response
                 ;; TODO body-params/explain ?
                 (resp/bad-request "Deserialisation error"))
          (throw error)))}))))

(defmacro defhandler
  "A drop-in replacement for pedestal's equivalent interceptor. Makes
  it simple to attach a meta tag holding the interceptor swagger
  documentation."
  [name doc args & body]
  `(def ~name
     (doc/annotate ~doc (interceptor/handler (keyword ~name) (fn ~args ~@body)))))

(defmacro defmiddleware
  "A drop-in replacement for pedestal's equivalent interceptor. Makes
  it simple to attach a meta tag holding the interceptor swagger
  documentation."
  [name doc before after]
  (let [f1 (cons 'fn before)
        f2 (cons 'fn after)]
    `(def ~name
       (doc/annotate ~doc (interceptor/middleware (keyword ~name) ~f1 ~f2)))))

(defmacro defon-request
  "A drop-in replacement for pedestal's equivalent interceptor. Makes
  it simple to attach a meta tag holding the interceptor swagger
  documentation."
  [name doc args & body]
  `(def ~name
     (doc/annotate ~doc (interceptor/on-request (keyword ~name) (fn ~args ~@body)))))

(defmacro defon-response
  "A drop-in replacement for pedestal's equivalent interceptor. Makes
  it simple to attach a meta tag holding the interceptor swagger
  documentation."
  [name doc args & body]
  `(def ~name
     (doc/annotate ~doc (interceptor/on-response (keyword ~name) (fn ~args ~@body)))))

(defmacro defaround
  "A drop-in replacement for pedestal's equivalent interceptor. Makes
  it simple to attach a meta tag holding the interceptor swagger
  documentation."
  [name doc before after]
  (let [f1 (cons 'fn before)
        f2 (cons 'fn after)]
    `(def ~name
       (doc/annotate ~doc (interceptor/around (keyword ~name) ~f1 ~f2)))))

(defmacro defbefore
  "A drop-in replacement for pedestal's equivalent interceptor. Makes
  it simple to attach a meta tag holding the interceptor swagger
  documentation."
  [name doc args & body]
  `(def ~name
     (doc/annotate ~doc (interceptor/before (keyword ~name) (fn ~args ~@body)))))

(defmacro defafter
  "A drop-in replacement for pedestal's equivalent interceptor. Makes
  it simple to attach a meta tag holding the interceptor swagger
  documentation."
  [name doc args & body]
  `(def ~name
     (doc/annotate ~doc (interceptor/after (keyword ~name) (fn ~args ~@body)))))

(defmacro defroutes
  "A drop-in replacement for pedestal's defroutes.  In addition to
  defining a var that holds the expanded routes, compiles the swagger
  documentation and injects it into the routes as a meta tag."
  ([name route-spec]
   `(defroutes ~name {} ~route-spec))
  ([name docs route-spec]
   `(let [route-table# (expand-routes (quote ~route-spec))]
      (def ~name (doc/inject-docs ~docs route-table#)))))
