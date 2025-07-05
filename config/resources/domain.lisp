(in-package :mu-cl-resources)

(setf *default-page-size* 100)
(setf *include-count-in-paginated-responses* t)
(setf *supply-cache-headers-p* t)
(setf sparql:*experimental-no-application-graph-for-sudo-select-queries* t)
(setf *cache-model-properties-p* t)
(setf mu-support::*use-custom-boolean-type-p* nil)
(setq *cache-count-queries-p* t)
(setf sparql:*query-log-types* nil) ;; hint: use app-http-logger for logging queries instead, all is '(:default :update-group :update :query :ask)


;; example
;; (define-resource dataset ()
;;   :class (s-prefix "dcat:Dataset")
;;   :properties `((:title :string ,(s-prefix "dct:title"))
;;                 (:description :string ,(s-prefix "dct:description")))
;;   :has-one `((catalog :via ,(s-prefix "dcat:dataset")
;;                       :inverse t
;;                       :as "catalog"))
;;   :has-many `((theme :via ,(s-prefix "dcat:theme")
;;                      :as "themes"))
;;   :resource-base (s-url "http://webcat.tmp.semte.ch/datasets/")
;;   :on-path "datasets")

(define-resource user ()
  :class (s-prefix "foaf:Person")
  :resource-base (s-url "https://auth-demo.redpencil.io/users/")
  :properties `((:first-name :string ,(s-prefix "foaf:firstName"))
                (:last-name :string ,(s-prefix "foaf:familyName")))
  :has-one `((account :via ,(s-prefix "foaf:account")
                       :as "account")
             (organization :via ,(s-prefix "org:member")
                       :inverse t
                       :as "organization"))
  :features '(include-uri)
  :on-path "users")

(define-resource account ()
  :class (s-prefix "foaf:OnlineAccount")
  :resource-base (s-url "https://auth-demo.redpencil.io/accounts/")
  :properties `((:provider :via ,(s-prefix "foaf:accountServiceHomepage"))
                (:roles :string-set ,(s-prefix "ext:sessionRole")))
  :has-one `((user :via ,(s-prefix "foaf:account")
                   :inverse t
                   :as "user"))
  :features '(include-uri)
  :on-path "accounts")

(define-resource organization ()
  :class (s-prefix "org:Organization")
  :resource-base (s-url "https://auth-demo.redpencil.io/organizations/")
  :properties `((:name :string ,(s-prefix "skos:prefLabel")))
  :has-many `((user :via ,(s-prefix "org:member")
                       :as "members"))
  :features '(include-uri)
  :on-path "organizations")

;; reading in the domain.json
(read-domain-file "domain.json")
