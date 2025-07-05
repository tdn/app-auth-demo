;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

;; (push (make-instance 'delta-logging-handler) *delta-handlers*) ;; enable if delta messages should be logged on terminal
(add-delta-messenger "http://delta-notifier/")
(setf *log-delta-messenger-message-bus-processing* nil) ;; set to t for extra messages for debugging delta messenger

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* nil) ; change nil to t for logging requests to virtuoso (and the response)
(setf *backend* "http://triplestore:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* nil) ; change nil to t for logging all incoming requests

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

(define-graph favorites ("http://mu.semte.ch/graphs/favorites/")
  ("ext:Favorite"
   -> "ext:book"
   -> "rdf:type"
   -> "mu:uuid"))

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

;;;;;;;;;;;;;
;; User roles

(supply-allowed-group "public")

(grant (read)
       :to-graph (public system)
       :for-allowed-group "public")

(supply-allowed-group "privatebooks"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?uuid WHERE {
            <SESSION_ID> ext:belongsToCompany/mu:uuid ?uuid
          }"
  :parameters ("uuid"))

(grant (read)
        :to-graph privatebooks
        :for-allowed-group "privatebooks")


(supply-allowed-group "favorites"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?uuid WHERE {
            <SESSION_ID> ext:belongsToCompany/mu:uuid ?uuid
          }"
  :parameters ("uuid"))

(grant (read write)
        :to-graph favorites
        :for-allowed-group "favorites")

;; (grant (read write)
;;        :to company
;;        :for "company")
