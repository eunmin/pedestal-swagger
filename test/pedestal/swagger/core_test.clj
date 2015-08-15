(ns pedestal.swagger.core-test
  (:require [pedestal.swagger.core :refer :all]
            [clojure.test :refer :all]
            [io.pedestal.test :refer :all]
            [schema.core :as s]
            [schema.test :as validation]
            [pedestal.swagger.doc :as doc]
            [pedestal.swagger.body-params :as body-params]
            [ring.util.response :refer [response]]
            [ring.swagger.swagger2 :as spec]
            [io.pedestal.http.route.definition :as definition]
            [io.pedestal.http.body-params :as pedestal-body-params]
            [io.pedestal.http :as bootstrap]
            [scjsv.core :as v]
            [clojure.java.io :as io]))

(use-fixtures :each validation/validate-schemas)

(def req s/required-key)

(defon-request auth-middleware
  {:description "Requires auth as header"
   :parameters {:header {(req "auth") s/Str}}}
  [req] req)

(defon-request id-middleware
  {:description "Requires id on path"
   :parameters {:path {:id s/Int}}}
  [req] req)

(defhandler put-handler
  {:summary "Put resource with id"
   :parameters {:body {:name s/Keyword}}}
  [{:keys [body-params path-params headers]}]
  (response {:params (merge body-params path-params
                            (select-keys headers ["auth"]))}))

(defhandler delete-handler
  {:summary "Delete resource with id"
   :parameters {:query {:notify s/Bool}}}
  [{:keys [query-params path-params headers]}]
  (response {:params (merge query-params path-params
                         (select-keys headers ["auth"]))}))

(defn non-documented-handler
  [req] (response {}))

(defhandler get-handler
  {:summary "Get all resources"
   :parameters {:query {:q s/Str}}
   :responses {200 {:schema {:status s/Str}}
               :default {:schema {:result [s/Str]}
                         :headers {(req "Location") s/Str}}}}
  [{:keys [query-params]}]
  (case (:q query-params)
    "ok" {:status 200 :body {:status "ok"}}
    "created" {:status 201
               :body {:result ["a" "b"]}
               :headers {"Location" "Here!"}}
    "fail" {:status 299 :body {:result "fail"}}))

(defn make-app [options]
  (-> options
      bootstrap/default-interceptors
      bootstrap/service-fn
      ::bootstrap/service-fn))

(definition/defroutes routes
  [["t" :test
    ["/" ^:interceptors [(body-params
                          (select-keys body-params/default-parser-map
                                       ["application/edn"]))
                         (coerce-request)
                         (validate-response)
                         auth-middleware]
     {:get get-handler}
     ["/x/:id" ^:interceptors [id-middleware]
      {:put put-handler
       :delete delete-handler
       :head non-documented-handler}]
     ["/doc" {:get [(swagger-json)]}]]]])

(def app (make-app {::bootstrap/router :prefix-tree
                    ::bootstrap/routes (doc/inject-docs
                                        {:title "Test"
                                         :version "0.1"} routes)}))

(deftest generates-correct-paths
  (let [paths {"/"
               {:get
                {:consumes ["application/edn"]
                 :description "Requires auth as header"
                 :summary "Get all resources"
                 :parameters {:query {:q s/Str}
                              :header {(req "auth") s/Str}}
                 :responses {200 {:schema {:status s/Str}}
                             400 {}
                             422 {}
                             500 {}
                             :default {:schema {:result [s/Str]}
                                       :headers {(req "Location") s/Str}}}}}
               "/x/:id"
               {:put
                {:consumes ["application/edn"]
                 :description "Requires id on path"
                 :summary "Put resource with id"
                 :parameters {:path {:id s/Int}
                              :header {(req "auth") s/Str}
                              :body {:name s/Keyword}}
                 :responses {400 {} 500 {} 422 {}}}
                :delete
                {:consumes ["application/edn"]
                          :description "Requires id on path"
                          :summary "Delete resource with id"
                          :parameters {:path {:id s/Int}
                                       :header {(req "auth") s/Str}
                                       :query {:notify s/Bool}}
                 :responses {400 {} 500 {} 422 {}}}}}]
    (is (= paths (doc/gen-paths routes)))))

(def validator (v/validator (slurp (io/resource "ring/swagger/v2.0_schema.json"))))

(deftest generates-valid-json-schema
  (validator (spec/swagger-json {:paths (doc/gen-paths routes)})))

(deftest coerces-params
  (are [resp req] (= resp (read-string (:body req)))
       {:params {"auth" "y", :id 1, :notify true}}
       (response-for app :delete "http://t/x/1?notify=true" :headers {"Auth" "y"})

       {:error {:headers {"auth" "missing-required-key"}}}
       (response-for app :delete "http://t/x/1?notify=true")

       {:error {:query-params {:notify "missing-required-key"}}}
       (response-for app :delete "http://t/x/1" :headers {"Auth" "y"})

       {:error {:path-params {:id "(not (integer? W))"}}}
       (response-for app :delete "http://t/x/W?notify=true" :headers {"Auth" "y"})

       {:error {:body-params {:name "(not (keyword? 3))"}}}
       (response-for app :put "http://t/x/1"
                     :headers {"Auth" "y"
                               "Content-Type" "application/edn"}
                     :body (pr-str {:name 3}))

       {:params {"auth" "y", :id 1, :name :foo}}
       (response-for app :put "http://t/x/1"
                     :headers {"Auth" "y"
                               "Content-Type" "application/edn"}
                     :body (pr-str {:name "foo"}))))

(deftest validates-response
  (are [status resp req] (and (= status (:status req))
                              (= resp (read-string (:body req))))
       200 {:status "ok"}
       (response-for app
                     :get "http://t/?q=ok"
                     :headers {"Auth" "y"})

       422 {:error {:query-params {:q "missing-required-key"}}}
       (response-for app
                     :get "http://t/"
                     :headers {"Auth" "y"})

       201 {:result ["a" "b"]}
       (response-for app
                     :get "http://t/?q=created"
                     :headers {"Auth" "y"})

       500 {:error {:headers {"Location" "missing-required-key"}
                    :body {:result "(not (sequential? \"fail\"))"}}}
       (response-for app
                     :get "http://t/?q=fail"
                     :headers {"Auth" "y"})))

(deftest returns-error-when-given-bad-body
  (is (= 400
         (:status (response-for app
                                :put "http://t/x/1"
                                :headers {"Auth" "y"
                                          "Content-Type" "application/edn"}
                                :body "{\"foo\" }")))))

(deftest checks-swagger-handler-like-any-other-route
  (are [resp req] (= resp (read-string (:body req)))
       {:error {:headers {"auth" "missing-required-key"}}}
       (response-for app :get "http://t/doc")))
