(ns citrine-challenge.handler
  (:require [compojure.core :refer :all]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [ring.middleware.json :as middleware]
            [citrine-challenge.parser :as parser]))

(defroutes app-routes
  (GET "/units" request
    (if-let [unit_string_expr (or (get-in request [:params :unit_string_expr])
                   (get-in request [:body :unit_string_expr]))]
      (if (parser/is-valid-input? (str unit_string_expr))
        {:status 200
         :body {:desc (str "The unit requested was " unit_string_expr)
              :si_equivalent (parser/get-conversion (str unit_string_expr))}}
        {:status 500
         :body {:message (str "The unit string " unit_string_expr " is not supported")}})
      {:status 404
        :body {:message (get-in request [:params :unit_string_expr])}}))
  (route/not-found "Not Found"))

(def app
  (-> (handler/site app-routes)
      (middleware/wrap-json-body {:keywords? true})
      middleware/wrap-json-response))
