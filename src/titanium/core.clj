(ns titanium.core
  (:require [clojure.string :as str])
  (:import (com.apicatalog.jsonld JsonLdOptions)
           (com.apicatalog.jsonld.document Document JsonDocument RdfDocument)
           (com.apicatalog.jsonld.http.media MediaType)
           (com.apicatalog.rdf RdfDataset)
           (jakarta.json JsonStructure)
           (java.io Reader InputStream StringReader)
           (java.net URI))
  (:refer-clojure :exclude [flatten]))

(defmacro cond-doto
  "`cond-doto` is to `doto` what `cond->` is to `->`."
  [x & clauses]
  (let [x-val (gensym "x")]
    `(let [~x-val ~x]
       (do
         ~@(map (fn [[test action]]
                  `(when ~test
                     ~(if (sequential? action)
                        `(~(first action) ~x-val ~@(rest action))
                        `(~action ~x-val))))
                (partition 2 clauses))
         ~x-val))))


(defn- remove-trailing-? [string]
  (cond-> string (str/ends-with? string "?") (subs 0 (dec (count string)))))

(defn- lisp-name->setter [sym]
  (-> sym
      name
      remove-trailing-?
      (str/split #"-")
      (->>
       (map str/capitalize)
       str/join
       (str ".set")
       symbol)))

(defmacro set-all-applicable [options-obj options-map keys]
  `(cond-doto ~options-obj
     ~@(mapcat
        (fn [key]
          (let [value `(get ~options-map ~key)]
            [value `(~(lisp-name->setter key) ~value)]))
        keys)))

(defn opts->JsonLdOptions
  ^JsonLdOptions
  [opts-map]
  (let [options (JsonLdOptions.)]
    (set-all-applicable
     options
     opts-map
     [:base :context-cache :document-cache :document-loader :embed
      :processing-mode :rdf-direction :compact-arrays? :compact-to-relative? :explicit?
      :extract-all-scripts? :numeric-id? :omit-default? :omit-graph? :ordered? :produce-generalized-rdf?
      :rdf-star? :required-all? :uri-validation? :use-native-types? :use-rdf-type?])

    (when-let [ctx (:expand-context opts-map)]
      (condp instance? (type ctx)
        Document (.setExpandContext options ^Document ctx)
        String (.setExpandContext options ^String ctx)
        URI (.setExpandContext options ^URI ctx)))

    options))

(defn- lisp-fn->java-method [fn-name]
  (-> fn-name
      name
      (str/split #"-")
      (->> ((fn [[fst & rst]] (str fst (str/join (map str/capitalize rst))))))
      symbol))

(defmacro def-for-types [function docstring param-syms type-colls]
  `(do
     (defmulti ~function ~docstring (fn [~@param-syms & _opts#] (mapv type ~param-syms)))
     ~@(for [types type-colls]
         (let [method (symbol (str "com.apicatalog.jsonld.JsonLd/" (lisp-fn->java-method function)))
               params-with-types (map #(with-meta %1 {:tag %2}) param-syms types)]
           `(defmethod ~function ~types
              [~@params-with-types ~'& {:as opts#}]
              (.. (~method ~@param-syms)
                  (~'options (opts->JsonLdOptions opts#))
                  ~'(get)))))))

(def-for-types compact
  "Compact a document with the given context"
  [document context]
  [[Document Document]
   [Document String]
   [Document URI]
   [String Document]
   [String String]
   [String URI]
   [URI Document]
   [URI String]
   [URI URI]])


(def-for-types expand
  "Expand a document"
  [document]
  [[Document] [String] [URI]])

(def-for-types flatten
  "Flatten a document"
  [document]
  [[Document] [String] [URI]])

(def-for-types frame
  "Frame a document"
  [document frame]
  [[Document Document]
   [Document String]
   [Document URI]
   [String Document]
   [String String]
   [String URI]
   [URI Document]
   [URI String]
   [URI URI]])

(def-for-types to-rdf
  "Turn a JSON-LD object into an RDF document"
  [document]
  [[Document] [String] [URI]])

(def-for-types from-rdf
  "Turn an RDF document into a JSON-LD object"
  [document]
  [[Document] [String] [URI]])


(defn json-document
  "Get a `JsonDocument` from some source, optionally with a specific media type.

  The source can be one of:
  - String (String is read as JSON)
  - InputStream
  - Reader
  - jakarta `JsonStructure`

  Default media type is `MediaType/JSON`"
  (^JsonDocument
   [doc-origin ^MediaType media-type]
   (condp instance? doc-origin
     String (JsonDocument/of media-type ^Reader (StringReader. doc-origin))
     InputStream (JsonDocument/of media-type ^InputStream doc-origin)
     Reader (JsonDocument/of media-type ^Reader doc-origin)
     JsonStructure (JsonDocument/of media-type ^JsonStructure doc-origin)))
  (^JsonDocument
   [doc-origin]
   (json-document doc-origin MediaType/JSON)))

(defn rdf-document
  "Get a `RdfDocument` from some source, optionally with a specific media type.

  The source can be one of:
  - String (String is read as RDF)
  - InputStream
  - Reader
  - `RdfDataset`

  Default media type is `MediaType/N_QUADS`"
  (^RdfDocument
   [doc-origin ^MediaType media-type]
   (condp instance? doc-origin
     String (RdfDocument/of media-type ^Reader (StringReader. doc-origin))
     InputStream (RdfDocument/of media-type ^InputStream doc-origin)
     Reader (RdfDocument/of media-type ^Reader doc-origin)
     RdfDataset (RdfDocument/of media-type ^RdfDataset doc-origin)))
  (^RdfDocument
   [doc-origin]
   (rdf-document doc-origin MediaType/N_QUADS)))
