(ns citrine-challenge.handler
  (:require [compojure.core :refer :all]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [ring.middleware.json :as middleware]))

(defroutes app-routes
  (POST "/" request
    (let [name (or (get-in request [:params :name])
                   (get-in request [:body :name])
                    "Morx Gorlont")]
      {:status 200
       :body {:name name
              :desc (str "The I got was " name)}}))
  (route/not-found "Not Found"))

(def app
  (-> (handler/site app-routes)
      (middleware/wrap-json-body {:keywords? true})
      middleware/wrap-json-response))
