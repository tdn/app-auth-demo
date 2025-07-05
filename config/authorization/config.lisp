;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

;; (push (make-instance 'delta-logging-handler) *delta-handlers*) ;; enable if delta messages should be logged on terminal
(add-delta-messenger "http://delta-notifier/")
(setf *log-delta-messenger-message-bus-processing* nil) ;; set to t for extra messages for debugging delta messenger

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* t) ; change nil to t for logging requests to virtuoso (and the response)
(setf *backend* "http://triplestore:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* t) ; change nil to t for logging all incoming requests

;;;;;;;;;;;;;;;;
;;; prefix types
(in-package :type-cache)

(add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session") ; each session URI will be handled for updates as if it had this mussession:Session type

;;;;;;;;;;;;;;;;;
;;; access rights

(in-package :acl)

;; these three reset the configuration, they are likely not necessary
(defparameter *access-specifications* nil)
(defparameter *graphs* nil)
(defparameter *rights* nil)

;; Prefixes used in the constraints below (not in the SPARQL queries)
(define-prefixes
  ;; Core
  :mu "http://mu.semte.ch/vocabularies/core/"
  :session "http://mu.semte.ch/vocabularies/session/"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :schema "http://schema.org/"
  :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  :dct "http://purl.org/dc/terms/"
  :owl "http://www.w3.org/2002/07/owl#"
  ;; Custom prefix URIs here, prefix casing is ignored
  :foaf "http://xmlns.com/foaf/0.1/"
  :org "http://www.w3.org/ns/org#"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :nfo "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
  :dbpedia "http://dbpedia.org/resource/"
  :nie "http://www.semanticdesktop.org/ontologies/2007/01/19/nie#"
  :service "http://services.semantic.works/"
  )


;;;;;;;;;
;; Graphs
;;
;; These are the graph specifications known in the system.  No
;; guarantees are given as to what content is readable from a graph.  If
;; two graphs are nearly identitacl and have the same name, perhaps the
;; specifications can be folded too.  This could help when building
;; indexes.

(define-graph public ("http://mu.semte.ch/graphs/public")
  (_ -> _)) ; public allows ANY TYPE -> ANY PREDICATE in the direction
            ; of the arrow

;; Example:
(define-graph privatebooks ("http://mu.semte.ch/graphs/privatebooks")
  ("schema:Book"
   -> "schema:genre"
   -> "dct:creator"
   -> "dct:issued"
   -> "owl:sameAs"))

(define-graph favorites ("http://mu.semte.ch/graphs/favorites")
  ("foaf:Person"
   -> "ext:hasFavorite"))

(define-graph system ("http://mu.semte.ch/graphs/system")
  ("foaf:Person"
   -> "mu:uuid"
   -> "foaf:firstName"
   -> "foaf:familyName"
   -> "foaf:account")
  ("foaf:OnlineAccount"
   -> "mu:uuid"
   -> "foaf:accountServiceHomepage"
   -> "foaf:accountName")
  ("org:Membership"
   -> "mu:uuid"
   -> "org:member"
   -> "org:role")
  ("org:Role"
   -> "mu:uuid"
   -> "skos:prefLabel"
   -> "skos:notation"
   -> "skos:definition"))

(define-graph qr-codes ("http://mu.semte.ch/graphs/qr-codes")
  ("nfo:FileDataObject"
   -> "mu:uuid"
   -> "rdf:type"
   -> "nfo:fileName"
   -> "dct:format"
   -> "nfo:fileSize"
   -> "dbpedia:fileExtension"
   -> "dct:created"
   -> "nie:dataSource"
   -> "schema:embeddedTextCaption"
   <- "ext:qrCode"))

;;;;;;;;;;;;;
;; User roles

(supply-allowed-group "public")

(grant (read)
       :to-graph (public system qr-codes)
       :for-allowed-group "public")

(supply-allowed-group "privatebooks"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
PREFIX session: <http://mu.semte.ch/vocabularies/session/>
          SELECT DISTINCT ?account WHERE {
            <SESSION_ID>  session:account ?account .
            ?account  ext:sessionRole <https://authorization-demo.redpencil.io/roles/32c2b6c3-4c99-462c-a145-083003eb95b5> .
          }")

(grant (read)
        :to-graph privatebooks
        :for-allowed-group "privatebooks")


(supply-allowed-group "favorites"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
PREFIX session: <http://mu.semte.ch/vocabularies/session/>
          SELECT DISTINCT ?account WHERE {
            <SESSION_ID>  session:account ?account .
            ?account  ext:sessionRole <https://authorization-demo.redpencil.io/roles/ed5396a4-2a1d-4624-8e79-df294843a0f8> .
          }")

(grant (read write)
       :to-graph favorites
       :for-allowed-group "favorites")

(with-scope "service:qrencode"
  (grant (read write)
         :to-graph qr-codes
         :for-allowed-group "public")
  (grant (read)
         :to-graph favorites
         :for-allowed-group "public"))

;; (grant (read write)
;;        :to company
;;        :for "company")
