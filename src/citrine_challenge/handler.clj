(ns citrine-challenge.handler
  (:require [compojure.core :refer :all]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [ring.middleware.json :as middleware]
            [citrine-challenge.units :as units]))

(defroutes app-routes
  (GET "/units" request
    (if-let [name (or (get-in request [:params :name])
                   (get-in request [:body :name]))]
      (if-let [matched-unit (get units/si (str name))]
        {:status 200
         :body {:desc (str "The unit requested was " name)
              :si_equivalent (get units/si (str name))}}
        {:status 404
         :body {:message (str "The unit " name " is not supported")}})
      {:status 404
        :body {:message "That unit is not supported"}}))
  (route/not-found "Not Found"))

(def app
  (-> (handler/site app-routes)
      (middleware/wrap-json-body {:keywords? true})
      middleware/wrap-json-response))
