(ns alanlcode.util.os-test
  (:require [alanlcode.util.os :refer :all :as os]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest]]
            [expectations :refer :all])
  (:import [clojure.lang ExceptionInfo]
           [java.nio.file FileAlreadyExistsException Path NoSuchFileException]))

(def test-input-path (->path "test-inputs" "os"))
(def test-input-contents (->> ["symlink" "regular_file" "dir_symlink" "directory"]
                              (map (partial ->path test-input-path))
                              (set)))

(def test-temp-dir (->path (temp-directory) "alanlcode.util"))

;; path-segments

(expect [(->path "")] (path-segments (->path "")))
(expect [(->path "1")] (path-segments (->path "1")))
(expect [(->path "1") (->path "2")] (path-segments (->path "1" "2")))
(expect [(->path "1") (->path "2") (->path "3")] (path-segments (->path "1" "2" "3")))

;; path-exists

(expect true (path-exists test-input-path))
(expect false (path-exists "doesntexist"))

;; is-absolute

(expect true (is-absolute (->absolute-path ".")))
(expect false (is-absolute (->path ".")))

;; is-regular

(expect true  (is-regular (->path test-input-path "regular_file")))
(expect false (is-regular (->path test-input-path "symlink")))
(expect false (is-regular (->path test-input-path "dir_symlink")))
(expect false (is-regular (->path test-input-path "directory")))
(expect false (is-regular (->path test-input-path ".")))
(expect false (is-regular (->path test-input-path "..")))
(expect false (is-regular "doesntexist"))

;; is-symlink

(expect false (is-symlink (->path test-input-path "regular_file")))
(expect true  (is-symlink (->path test-input-path "symlink")))
(expect true  (is-symlink (->path test-input-path "dir_symlink")))
(expect false (is-symlink (->path test-input-path "directory")))
(expect false (is-symlink (->path test-input-path ".")))
(expect false (is-symlink (->path test-input-path "..")))
(expect false (is-symlink "doesntexist"))

;; is-directory

(expect false (is-directory (->path test-input-path "regular_file")))
(expect true  (is-directory (->path test-input-path "symlink")))
(expect false (binding [*follow-links* false] (is-directory (->path test-input-path "symlink"))))
(expect true  (is-directory (->path test-input-path "dir_symlink")))
(expect false (binding [*follow-links* false] (is-directory (->path test-input-path "dir_symlink"))))
(expect true  (is-directory (->path test-input-path "directory")))
(expect true  (is-directory (->path test-input-path ".")))
(expect true  (is-directory (->path test-input-path "..")))
(expect false (is-directory "doesntexist"))

;; directory-seq

(expect test-input-contents (set (directory-seq test-input-path)))
(expect empty? (directory-seq "doesntexist"))

;; recursive-directory-seq

(expect (count test-input-contents)
        (count (set/intersection test-input-contents
                                 (set (recursive-directory-seq test-input-path)))))
(expect (->path test-input-path "directory" "regular_file")
        (in (set (recursive-directory-seq test-input-path))))
(expect empty? (recursive-directory-seq "doesntexist"))


;; child-directories

(expect #{(->path test-input-path "directory")
          (->path test-input-path "dir_symlink")
          (->path test-input-path "symlink")}
        (set (child-directories test-input-path)))
(expect [(->path test-input-path "directory")] (binding [*follow-links* false] (child-directories test-input-path)))
(expect empty? (child-directories "doesntexist"))

;; child-files

(expect [(->path test-input-path "regular_file")] (child-files test-input-path))
(expect empty? (child-files "doesntexist"))

;; child-directories

(expect (->path (current-directory) "src") (in (child-directories (current-directory))))
(expect (->path (current-directory) "test") (in (child-directories (current-directory))))
(expect empty? (child-directories "doesntexist"))

;; child-files

(expect empty? (child-files "doesntexist"))
(expect (->path test-input-path "regular_file") (in (child-files test-input-path)))
(expect false (contains? (set (child-files test-input-path)) (->path test-input-path "directory")))
(expect false (contains? (set (child-files test-input-path)) (->path test-input-path "symlink")))

;; current-directory

(expect (->path (System/getProperty "user.dir")) (current-directory))

;; temp-directory

(expect (->path (System/getProperty "java.io.tmpdir")) (temp-directory))

;; nil-unless-exists

(expect (->path "/") (nil-unless-exists "/"))
(expect nil (nil-unless-exists "doesntexist"))

;; ->path

(expect Path (->path "/"))
(expect Path (->path "test" "alanlcode"))
(expect Path (->path (current-directory)))
(expect (current-directory) (->path (current-directory)))
(expect (.toString (io/file (System/getProperty "user.dir")))
        (.toString (->path (current-directory))))

;; make-file

(let [tmp-dir (->path (temp-directory) "make-file-test")]
  (expect tmp-dir (make-dir tmp-dir))
  (expect (more-of x
                   Path x
                   path-exists x
                   is-regular x) (make-file (->path tmp-dir "file")))
  (expect nil (make-file (->path tmp-dir "file")))  ;; already exists
  (expect nil (make-file (->path tmp-dir "1" "2"))) ;; parent dir missing
  (expect nil (recursive-delete-directory tmp-dir)))

;; make-dir

(expect (more-of x
                 Path x
                 path-exists x
                 is-directory x)
        (do
          (recursive-delete-directory test-temp-dir)
          (make-dir test-temp-dir)
          (make-dir (->path test-temp-dir "dir"))))

(expect nil
        (do
          (recursive-delete-directory test-temp-dir)
          (make-dir test-temp-dir)
          (make-dir test-temp-dir))) ;; dir already exists

(expect nil
        (do
          (recursive-delete-directory test-temp-dir)
          (make-dir test-temp-dir)
          (make-dir (->path test-temp-dir "dir"))
          (make-dir (->path test-temp-dir "1" "2")))) ;; parent dir '1' does not exist

(expect (->path test-temp-dir "file")
        (do
          (recursive-delete-directory test-temp-dir)
          (make-dir test-temp-dir)
          (make-file (->path test-temp-dir "file"))))

(expect nil
        (do
          (recursive-delete-directory test-temp-dir)
          (make-dir test-temp-dir)
          (make-file (->path test-temp-dir "file"))
          (make-dir (->path test-temp-dir "file")))) ;; file 'file' already exists

;; make-or-get-dir

(expect test-input-path (make-or-get-dir test-input-path))
(expect nil (make-or-get-dir (->path test-input-path "regular_file"))) ;; same name as regular file
(expect (->path test-input-path "symlink") (make-or-get-dir (->path test-input-path "symlink"))) ;; same name as symlink
(expect nil (binding [*follow-links* false] (make-or-get-dir (->path test-input-path "symlink")))) ;; same name as symlink
(expect (->path test-input-path "directory") (make-or-get-dir (->path test-input-path "directory")))

;; make-or-get-dirs

(expect (more path-exists (->path test-temp-dir "1" "2" "3" "4"))
        (do
          (recursive-delete-directory test-temp-dir)
          (make-dir test-temp-dir)
          (make-or-get-dirs (->path test-temp-dir "1" "2" "3" "4"))
          (make-or-get-dirs (->path test-temp-dir "1" "2" "3" "4"))))

(expect nil
        (do
          (recursive-delete-directory test-temp-dir)
          (make-dir test-temp-dir)
          (make-or-get-dirs (->path test-temp-dir "1" "2" "3" "4"))
          (make-file (->path test-temp-dir "1" "2" "3" "file"))
          (make-or-get-dirs (->path test-temp-dir "1" "2" "3" "file"))))

;; make-temp-file

(expect (more-of x
                 Path x
                 #"output/hello" (.toString x)
                 path-exists x
                 is-regular x)
        (make-temp-file "hello"))

(expect (more-of x
                 Path x
                 #"output/hello-1-world" (.toString x)
                 path-exists x
                 is-regular x)
        (->> #(make-temp-file "output" "hello" "-world")
             (repeatedly 2)
             (last)))

(let [root (->path (temp-directory) "alanlcode.util")]
  (expect (more-of x
                   Path x
                   #"alanlcode.util/hello-4-world$" (.toString x)
                   path-exists x
                   is-regular x)
          (->> #(make-temp-file root "hello" "-world")
               (repeatedly 5)
               (last)))
  (expect nil (recursive-delete-directory root)))

;; make-temp-dir

(expect (more-of x
                 Path x
                 #"^output/hellodir" (.toString x)
                 path-exists x
                 is-directory x)
        (make-temp-dir "hellodir"))

(expect (more-of x
                 Path x
                 "output/hellodir-1-world" (.toString x)
                 path-exists x
                 is-directory x)
        (->> #(make-temp-dir "output" "hellodir" "-world")
             (repeatedly 2)
             (last)))

(let [root (->path (temp-directory) "alanlcode.util")]
  (expect (more-of x
                   Path x
                   #"alanlcode.util/hellodir-4-world$" (.toString x)
                   path-exists x
                   is-directory x)
          (->> #(make-temp-dir root "hellodir" "-world")
               (repeatedly 5)
               (last)))
  (expect nil (recursive-delete-directory root)))

(expect nil (recursive-delete-directory "output"))

;; file-relative-to
;; TODO: finish this

(expect (->path "a") (path-relative-to "" "a"))
(expect (->path "a") (path-relative-to "/" "/a"))
(expect (->path "c") (path-relative-to "a/b" "a/b/c"))
(expect (->path "c") (path-relative-to "/a/b" "/a/b/c"))
(expect (->path "/a/b/c") (path-relative-to "/a/b/c/d" "/a/b/c"))

(expect (into []
              (for [rbit [false true]
                    wbit [false true]
                    xbit [false true]
                    own  [false true]
                    :let [fperms [rbit wbit xbit]
                          lperms fperms]]
                [fperms lperms]))

        (do (recursive-delete-directory test-temp-dir)
            (let [root (make-dir test-temp-dir)
                  fls (doall
                        (for [rbit [false true]
                              wbit [false true]
                              xbit [false true]
                              own  [false true]
                              :let [bitmask-str (str (if rbit "r" "_")
                                                     (if wbit "w" "_")
                                                     (if xbit "x" "_")
                                                     (if own  "o" "_"))
                                    f (make-file (->path root (str "f-" bitmask-str)))
                                    l (create-symlink (->path root (str "l-" bitmask-str)) f)
                                    set-readable   (if own os/set-owner-readable os/set-readable)
                                    set-writable   (if own os/set-owner-writable os/set-writable)
                                    set-executable (if own os/set-owner-executable os/set-executable)]]
                          (do (set-posix-permissions f "000")
                              (if rbit (set-readable f) (set-unreadable f))
                              (if wbit (set-writable f) (set-unwritable f))
                              (if xbit (set-executable f) (set-unexecutable f))
                              [f l])))
                  permission-f (juxt is-readable is-writable is-executable)]
              (into []
                    (map (fn [[f l]] [(permission-f f) (permission-f l)]))
                    fls))))

(expect (into [] 
              (map #(format "%03o" %))
              (range (inc 0777)))
        (do (recursive-delete-directory test-temp-dir)
            (let [root (make-dir test-temp-dir)
                  fs (doall
                       (for [poctal (range (inc 0777))
                             :let [f (make-file (->path root (str "f-" (format "%03o" poctal))))]]
                         (do (set-posix-permissions f (format "%03o" poctal))
                             f)))]
              (into []
                    (map get-posix-permissions)
                    fs))))

(expect ExceptionInfo (set-posix-permissions test-temp-dir "abc"))
(expect ExceptionInfo (set-posix-permissions test-temp-dir ""))
(expect ExceptionInfo (set-posix-permissions test-temp-dir "1"))
