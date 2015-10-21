(ns alanlcode.util.os
  "Generic OS-interaction related utilities."
  (:require [clj-time
             [coerce :as convert]
             [core :as time]]
            [clojail.core :refer (thunk-timeout)]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.file FileAlreadyExistsException Files NotLinkException]
           java.nio.file.attribute.FileAttribute
           java.security.MessageDigest))

(defn nil-unless-exists
  "Return the given file if it actually exists, nil otherwise."
  [file]
  (when (.exists file)
    file))

(defn gen-timestamp []
  (convert/to-long (time/now)))

(defn child-directories [parent-dir]
  (let [parent-dir (io/file parent-dir)]
    (for [f (.listFiles parent-dir)
          :when (.isDirectory f)]
      f)))

(defn child-files [parent-dir]
  (let [parent-dir (io/file parent-dir)]
    (for [f (.listFiles parent-dir)
          :when (.isFile f)]
      f)))

(defn as-path [file]
  (.toPath (io/file file)))

(defn make-temp
  ([prefix file-or-dir]
   (make-temp prefix "" file-or-dir))
  ([prefix suffix file-or-dir]
   (let [make (case file-or-dir
                :file #(Files/createFile % (make-array FileAttribute 0))
                :dir  #(Files/createDirectory % (make-array FileAttribute 0))
                (throw (Exception. (str "Unrecognized option: " file-or-dir))))]
     (try
       (Files/createDirectory (as-path "output")
                              (make-array FileAttribute 0))
       (catch FileAlreadyExistsException e nil)) ; it's fine if it already exists

     (let [idx (atom 0)
           success (atom false)
           ret (atom nil)]
       (while (not @success)
         (try
           (if (= 0 @idx)
             (let [path (make (as-path (io/file "output" (str prefix suffix))))]
               (reset! success true)
               (reset! ret (.toFile path)))
             (let [path (make (as-path (io/file "output" (str prefix "-" (format "%d" @idx) suffix))))]
               (reset! success true)
               (reset! ret (.toFile path))))
           (catch FileAlreadyExistsException e
             (if (< @idx 9999)
               (swap! idx inc)
               (throw e)))))
       @ret))))

(defn make-temp-file
  "Create a temporary file."
  ([prefix] (make-temp prefix :file))
  ([prefix suffix] (make-temp prefix suffix :file)))

(defn make-temp-dir
  "Create a temporary directory."
  ([prefix] (make-temp prefix :dir))
  ([prefix suffix] (make-temp prefix suffix :dir)))

(defn make-dir
  "Create a new directory whose path consists of root-dir followed by
   path-segments, separated by path separator.  Return nil if
   directory could not be created, otherwise return the File."
  [root-dir & path-segments]
  (let [new-dir (apply io/file root-dir path-segments)]
    (try
      (.toFile (Files/createDirectories (as-path new-dir) (make-array FileAttribute 0)))
      (catch FileAlreadyExistsException e
        nil))))

(defn make-or-get-dir
  "Create a new directory whose path consists of root-dir followed by
   path-segments, separated by path separator.  Will throw exception is fails:
   see Files/createDirectory."
  [root-dir & path-segments]
  (let [new-dir (apply io/file root-dir path-segments)]
    (try
      (.toFile (Files/createDirectory (as-path new-dir) (make-array FileAttribute 0)))
      (catch FileAlreadyExistsException e
        (if (.isDirectory new-dir)
          new-dir
          (throw e))))))

(defn make-or-get-dirs
  "Like make-or-get-dir, but will attempt to create all intermediate directories
   if they do not already exist. Will throw exception if any such directory
   cannot be created. Note that File/createDirectories cannot be used here
   because it will silently fail."
  [root-dir & path-segments]
  (let [dirs (for [n (range (inc (count path-segments)))]
               (->> (take n path-segments)
                    (apply io/file root-dir)))]
    (last (doall (map make-or-get-dir dirs)))))

(defn file-relative-to
  "Return a File with relative path from from-file to to-file."
  [from-file to-file]
  (let [from-uri (.toURI (io/file from-file))
        to-uri (.toURI (io/file to-file))]
    (-> (.relativize from-uri to-uri)
        (.getPath)
        (io/file))))

(defn path-components
  "Return each path segment in a file's path, as a vector of strings."
  [file]
  (-> (io/file file)
      (.getPath)
      (str/split (re-pattern java.io.File/separator))))

(defn name-and-ext
  "Return the name and extension of a file, as a vector of strings [name ext].
   ext does not include the separator."
  [file]
  (let [fname (.getName (io/file file))
        sep-index (.lastIndexOf fname (int \.))]
    (if (>= sep-index 0)
      [(subs fname 0 sep-index)
       (subs fname (inc sep-index))]
      [fname ""])))

(defn resolve-symlink
  "Resolve the target of this symlink.  If this is not a symlink, just return
   the file. Otherwise return nil if this fails."
  [file]
  (let [path (as-path file)]
    (try
      (.toFile (Files/readSymbolicLink path))
      (catch NotLinkException e
        (.toFile path))
      (catch Exception e
        nil))))

(defn create-symlink
  "Create a symbolic link.  Returns the link as a File if it was created, nil otherwise.
   Requires Java 7."
  [link target]
  (let [body (fn []
               (.toFile
                 (Files/createSymbolicLink (as-path link)
                                           (as-path target)
                                           (make-array FileAttribute 0))))]
    (try
      (body)
      (catch FileAlreadyExistsException e
        (io/delete-file link)
        (body))
      (catch Exception e nil))))

(defn copy-file
  "Copy a file into dest-dir."
  [file dest-dir]
  (let [src-file (io/file file)
        dest-file (io/file dest-dir (.getName src-file))]
    (io/copy src-file dest-file)))

(defn nested?
  "True if child is nested in ancestor directory, false otherwise."
  [ancestor child]
  (let [ancestor (.getCanonicalFile (io/file ancestor))
        child (.getCanonicalFile (io/file child))]
    (when (.isDirectory ancestor)
      (when-let [parent (.getParentFile child)]
        (if (.equals ancestor parent)
          true
          (recur ancestor parent))))))

(defn sha256-string [file]
  (when-let [contents (slurp file)]
    (let [hash-instance (doto (MessageDigest/getInstance "SHA-256")
                          (.update (.getBytes contents)))]
      (apply str (map #(format "%02x" %) (.digest hash-instance))))))

(defmacro with-timeout [ms & body]
  `(thunk-timeout (fn [] ~@body) ~ms))

(defmacro timed
  "Return how long the underlying forms took to execute in nanoseconds."
  ([& forms]
   `(let [s# (System/nanoTime)]
      ~@forms
      (- (System/nanoTime) s#))))
