(ns alanlcode.util.os
  "OS related utilities."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import java.io.File
           [java.net URI URL]
           [java.nio.file FileAlreadyExistsException Files LinkOption NotLinkException Path Paths]
           [java.nio.file.attribute FileAttribute PosixFilePermission]))

(def FOLLOW (into-array LinkOption nil))
(def NOFOLLOW (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(def ^:dynamic *follow-links* true)

(defn- link-options []
  (if *follow-links* FOLLOW NOFOLLOW))

(defn ^File ->file [& args]
  (apply io/file
         (map (fn [x] (cond (instance? Path x) (.toFile x)
                            (instance? URI x) (.getPath x)
                            (instance? URL x) (.getPath x)
                            :else x))
              args)))

(defn ^Path ->path 
  ([arg]
   (if (instance? Path arg) arg (.toPath (->file arg))))
  ([arg & args]
   (.toPath (apply ->file arg args))))

(defn ^URI ->uri
  ([arg]
   (if (instance? URI arg) arg (.toUri (->path arg))))
  ([arg & args]
   (.toUri (apply ->path arg args))))

(defn ^URL ->url 
  ([arg]
   (if (instance? URL arg) arg (.toURL (->uri arg))))
  ([arg & args]
   (.toURL (apply ->uri arg args))))

(defn ->absolute-path
  "Return absolute path."
  [path]
  (.toAbsolutePath (->path path)))

(defn path-segments [path]
  (let [path (->path path)]
    (iterator-seq (.iterator path))))

(defn path-exists
  "Return true if path exists.  Follows symlinks by default."
  [path]
  (Files/exists (->path path) (link-options)))

(defn is-absolute
  "Return true if path is absolute."
  [path]
  (.isAbsolute (->path path)))

(defn is-regular
  "Return true if path is a regular file. Follows symlinks by default."
  [path]
  (Files/isRegularFile (->path path) (link-options)))

(defn is-symlink
  "Return true if path is a symlink. Follows symlinks by default."
  [path]
  (Files/isSymbolicLink (->path path)))

(defn is-directory
  "Return true if path is a directory. Follows symlinks by default."
  [path]
  (Files/isDirectory (->path path) (link-options)))

(defn is-readable
  "Return true if path is readable."
  [path]
  (Files/isReadable (->path path)))

(defn is-writable
  "Return true if path is writable."
  [path]
  (Files/isWritable (->path path)))

(defn is-executable
  "Return true if path is executable."
  [path]
  (Files/isExecutable (->path path)))

(defn set-readable
  "Make file readable to all. Follows symlinks. Returns nil."
  [path]
  (.setReadable (->file path) true false)
  nil)

(defn set-owner-readable
  "Make file readable to owner. Follows symlinks. Returns nil."
  [path]
  (.setReadable (->file path) true true)
  nil)

(defn set-unreadable
  "Make file unreadable to all. Follows symlinks. Returns nil."
  [path]
  (.setReadable (->file path) false false)
  nil)

(defn set-owner-unreadable
  "Make file unreadable to owner. Follows symlinks. Returns nil."
  [path]
  (.setReadable (->file path) false true)
  nil)

(defn set-writable
  "Make file writable to all. Follows symlinks. Returns nil."
  [path]
  (.setWritable (->file path) true false)
  nil)

(defn set-owner-writable
  "Make file writable to owner. Follows symlinks. Returns nil."
  [path]
  (.setWritable (->file path) true true)
  nil)

(defn set-unwritable
  "Make file unwritable to all. Follows symlinks. Returns nil."
  [path]
  (.setWritable (->file path) false false)
  nil)

(defn set-owner-unwritable
  "Make file unwritable to owner. Follows symlinks. Returns nil."
  [path]
  (.setWritable (->file path) false true)
  nil)

(defn set-executable
  "Make file executable to all. Follows symlinks. Returns nil."
  [path]
  (.setExecutable (->file path) true false)
   nil)

(defn set-owner-executable
  "Make file executable to owner. Follows symlinks. Returns nil."
  [path]
  (.setExecutable (->file path) true true)
   nil)

(defn set-unexecutable
  "Make file unexecutable to all. Follows symlinks. Returns nil."
  [path]
  (.setExecutable (->file path) false false)
   nil)

(defn set-owner-unexecutable
  "Make file unexecutable to owner. Follows symlinks. Returns nil."
  [path]
  (.setExecutable (->file path) false true)
   nil)

(defn get-posix-permissions
  "Get permissions of file using POSIX symbolic notation. If successful, return
   a 3-character octal string, otherwise return nil."
  [path]
  (let [has-permission? (fn [coll p] (if (contains? coll p) 1 0))
        owner-fn (juxt #(has-permission? % PosixFilePermission/OWNER_READ)
                       #(has-permission? % PosixFilePermission/OWNER_WRITE)
                       #(has-permission? % PosixFilePermission/OWNER_EXECUTE))
        group-fn (juxt #(has-permission? % PosixFilePermission/GROUP_READ)
                       #(has-permission? % PosixFilePermission/GROUP_WRITE)
                       #(has-permission? % PosixFilePermission/GROUP_EXECUTE))
        other-fn (juxt #(has-permission? % PosixFilePermission/OTHERS_READ)
                       #(has-permission? % PosixFilePermission/OTHERS_WRITE)
                       #(has-permission? % PosixFilePermission/OTHERS_EXECUTE))
        bits->octal (fn [bits]
                      (reduce + (map (fn [b n] (* b (int (Math/pow 2 n)))) 
                                     (reverse bits)
                                     (range))))]
    (->> (Files/getPosixFilePermissions (->path path) (link-options))
         ((juxt owner-fn group-fn other-fn))
         (map bits->octal)
         (apply str))))

(defn set-posix-permissions
  "Set permissions of file using POSIX symbolic notation. permission-str must
   be a 3-character octal string; otherwise, throws ExceptionInfo. Returns
   nil."
  [path permission-str]
  (when (not= 3 (count permission-str))
    (throw (ex-info (str "Invalid permission string " permission-str)
                    {:permission-str permission-str
                     :cause ::invalid-permission-string})))
  (let [parse-fn 
        (fn [s]
          (try
            (let [n (Integer/parseUnsignedInt s 8)]
              (loop [ixs (range 9)
                     n n
                     perms '()]
                (if (seq ixs)
                  (recur (next ixs)
                         (bit-shift-right n 1)
                         (conj perms (bit-test n 0)))
                  perms)))
            (catch NumberFormatException e
              (throw (ex-info (str "Invalid permission string " permission-str)
                              {:permission-str permission-str
                               :cause ::invalid-permission-string})))))
        permission-order 
        [PosixFilePermission/OWNER_READ  PosixFilePermission/OWNER_WRITE  PosixFilePermission/OWNER_EXECUTE  
         PosixFilePermission/GROUP_READ  PosixFilePermission/GROUP_WRITE  PosixFilePermission/GROUP_EXECUTE  
         PosixFilePermission/OTHERS_READ PosixFilePermission/OTHERS_WRITE PosixFilePermission/OTHERS_EXECUTE]
        permissions
        (set
          (for [[p b] (map list permission-order (parse-fn permission-str))
                :when b]
            p))]
    (Files/setPosixFilePermissions (->path path) permissions)
    nil))

(defn directory-seq
  "Return seq of paths at root. Does not recurse. Never throws exception."
  [path]
  (try
    (iterator-seq (.iterator (Files/list (->path path))))
    (catch Exception _ nil)))

(defn child-directories
  "Return seq of child directory paths at root. Does not recurse. Never throws exception."
  [path]
  (doall (filter is-directory (directory-seq (->path path)))))

(defn child-files [path]
  "Return seq of regular file paths at root. Does not recurse. Never throws exception."
  (doall (filter is-regular (directory-seq (->path path)))))

(defn recursive-directory-seq
  "Return seq of paths at root. Recurses. Does not follow symlinks."
  [root]
  (let [path (->path root)]
    (cond
      (not (path-exists path))
      nil
      (or (is-regular path) (is-symlink path))
      (list path)
      :else
      (doall (concat (list path) (mapcat recursive-directory-seq (directory-seq path)))))))

(defn ^Path current-directory
  "Return path to current directory."
  []
  (->path (System/getProperty "user.dir")))

(defn ^Path temp-directory
  "Return path to system temp dir."
  []
  (->path (System/getProperty "java.io.tmpdir")))

(defn nil-unless-exists
  "Return this path if it actually exists, nil otherwise."
  [path]
  (when (path-exists (->path path))
    (->path path)))

(defn- -make-file [path]
  (Files/createFile (->path path) (make-array FileAttribute 0)))

(defn- -make-dir [path]
  (Files/createDirectory (->path path) (make-array FileAttribute 0)))

(defn make-file
  "Create a new file. Return the path if successful, otherwise nil. Never throws
  exception."
  [path]
  (try
    (-make-file path)
    (catch Exception _ nil)))

(defn make-dir
  "Create a new directory. Return the path if successful, otherwise nil. Never
  throws exception."
  [path]
  (try
    (-make-dir path)
    (catch Exception _ nil)))

(defn make-or-get-dir
  "Create a new directory if it doesn't exist. If directory was created
  successfully, or directory already exists, then return the path, otherwise
  return nil. Never throws exception."
  [path]
  (let [path (->path path)]
    (try
      (-make-dir path)
      (catch FileAlreadyExistsException _
        (if (is-directory path)
          path
          nil))
      (catch Exception _ nil))))

(defn make-or-get-dirs
  "Like make-or-get-dir, but attempt to create all intermediate directories if
  they do not already exist. Will return nil if a directory does not exist but
  cannot be created. Never throws exception. Note that this does not have the
  same semantics as File/createDirectories, which stops the first time it
  encounters a directory that already exists. This function continues to create
  additional directories as necessary."
  [path]
  (let [path (->path path)
        from-segs (fn [segs]
                    (if (is-absolute path)
                      (apply ->path "/" segs)
                      (apply ->path segs)))
        segments (path-segments path)
        dirs (for [n (range 1 (inc (count segments)))]
               (from-segs (take n segments)))]
    (last (doall (map make-or-get-dir dirs)))))

(defn ^Path make-temp
  "Make a temporary file or directory and return its path. Defaults to root
  directory 'output' in the current directory unless otherwise specified. Throws
  FileAlreadyExistsException after 10000 failed attempts."
  ([prefix file-or-dir]
   (make-temp "output" prefix "" file-or-dir))
  ([root prefix file-or-dir]
   (make-temp root prefix "" file-or-dir))
  ([root prefix suffix file-or-dir]
   (let [make (case file-or-dir
                :file -make-file
                :dir  -make-dir
                (throw (RuntimeException. (str "Unrecognized option: " file-or-dir))))
         root-path (try
                    (-make-dir (->path root))
                    (catch FileAlreadyExistsException _ ;; it's fine if it already exists
                      (->path root)))]

     (let [idx (atom 0)
           success (atom false)
           ret (atom nil)]
       (while (not @success)
         (try
           (if (= 0 @idx)
             (let [path (make (->path root-path (str prefix suffix)))]
               (reset! success true)
               (reset! ret path))
             (let [path (make (->path root-path (str prefix "-" (format "%d" @idx) suffix)))]
               (reset! success true)
               (reset! ret path)))
           (catch FileAlreadyExistsException e
             (if (< @idx 9999)
               (swap! idx inc)
               (throw e)))))
       @ret))))

(defn make-temp-file
  "Create a temporary file and return its path. Defaults to root directory
  'output' in the current directory unless otherwise specified. Throws
  FileAlreadyExistsException after 10000 failed attempts."
  ([prefix] (make-temp prefix :file))
  ([root prefix] (make-temp root prefix :file))
  ([root prefix suffix] (make-temp root prefix suffix :file)))

(defn make-temp-dir
  "Create a temporary directory and return its path. Defaults to root directory
  'output' in the current directory unless otherwise specified. Throws
  FileAlreadyExistsException after 10000 failed attempts."
  ([prefix] (make-temp prefix :dir))
  ([root prefix] (make-temp root prefix :dir))
  ([root prefix suffix] (make-temp root prefix suffix :dir)))

(defn file-relative-to
  "Return a File with relative path from from-file to to-file."
  [from-file to-file]
  (let [from-uri (.toURI (->file from-file))
        to-uri (.toURI (->file to-file))]
    (-> (.relativize from-uri to-uri)
        (.getPath)
        (->file))))

(defn path-relative-to
  "Return a relative Path from from-path to to-path. from-path must be a prefix of to-path, otherwise
  to-path is simply returned."
  [from-path to-path]
  (->path (.relativize (->uri from-path) (->uri to-path))))

(defn name-and-ext
  "Return the name and extension of a file, as a vector of strings [name ext].
   ext does not include the separator."
  [file]
  (let [fname (.getName (->file file))
        sep-index (.lastIndexOf fname (int \.))]
    (if (>= sep-index 0)
      [(subs fname 0 sep-index)
       (subs fname (inc sep-index))]
      [fname ""])))

(defn has-extension?
  "Return true iff the given file has the given extension (excluding the . separator)"
  [extension file]
  (let [[_ ext] (name-and-ext file)]
    (= ext extension)))

(defn resolve-symlink
  "Resolve the target of this symlink.  If this is not a symlink, just return
   the file. Otherwise return nil if this fails."
  [file]
  (let [path (->path file)]
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
                 (Files/createSymbolicLink (->path link)
                                           (->path target)
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
  (let [src-file (->file file)
        dest-file (->file dest-dir (.getName src-file))]
    (io/copy src-file dest-file)))

(defn nested?
  "True if child is nested in ancestor directory, false otherwise."
  [ancestor child]
  (let [ancestor (.getCanonicalFile (->file ancestor))
        child (.getCanonicalFile (->file child))]
    (when (.isDirectory ancestor)
      (when-let [parent (.getParentFile child)]
        (if (.equals ancestor parent)
          true
          (recur ancestor parent))))))

(defn delete-file [path]
  (Files/delete (->path path)))

(defn recursive-delete-directory
  "Delete directory. Recursively delete contents if necessary."
  [root]
    (doseq [path (reverse (recursive-directory-seq root))]
      (delete-file path)))

