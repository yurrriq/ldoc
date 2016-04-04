(defmodule ldoc
  (doc "`\"LDoc\"` chunk utility functions.")
  (export (doc 1) (doc 3)
          (docs 1) (exports 1) (module 1)))

(include-lib "lfe/src/lfe_doc.hrl")

(defun doc
  ([`#(,mod ,func ,arity)]
   (doc mod func arity)))

(defun doc (mod func arity)
  (case (docs mod)
    (`#(ok ,docs)
     (case (lists:filter (lambda (doc) (=:= (doc-name doc) func)) docs)
       ([]      #(error not-found))
       (matches
        (case (lists:keyfind arity 5 matches)
          ('false (found mod matches))
          (doc    (pprint mod doc))))))
    (other other)))

(defun docs
  ([(match-lfe_docs_v1 docs docs)]
   `#(ok ,docs))
  ([mod-or-beam] (when (orelse (is_atom mod-or-beam)
                               (is_binary mod-or-beam)
                               (is_list mod-or-beam)))
   (case (module mod-or-beam)
     (`#(ok ,ldoc) (docs ldoc))
     (other        other)))
  ([_]
   #(error badarg)))

(defun exports
  ([docs] (when (is_list docs))
   `#(ok ,(lists:filter #'exported?/1 docs)))
  ([(match-lfe_docs_v1 docs docs)]
   (exports docs))
  ([mod] (when (is_atom mod))
   (case (docs mod)
     (`#(ok ,docs) (exports docs))
     (other        other)))
  ([_]
   #(error badarg)))

(defun module
  "Return a given `beam` module's `\"LDoc\"` chunk as a term.
  If the chunk is missing, return `#(error #\"Missing \"LDoc\" chunk.\")`."
  ([beam] (when (orelse (is_binary beam) (is_list beam)))
   (case (beam_lib:chunks beam '["LDoc"] '[allow_missing_chunks])
     (`#(ok #(,_ [#("LDoc" missing_chunk)]))
      #(error missing-ldoc-chunk))
     (`#(ok #(,_ [#("LDoc" ,chunk)]))
      (binary->term chunk))
     (`#(error beam_lib ,reason)
      `#(error ,reason))))
  ([mod] (when (is_atom mod))
   (case (code:get_object_code mod)
     ('error                 #(error non-existing))
     (`#(,_mod ,beam ,_file) (module beam)))))

(defun pprint
  ([mod (match-doc name name arity arity patterns patterns doc doc)]
   (let* ((format "-------------------------\n~p:~p/~p\n(~s)\n  ~s\n")
          (patterns* (string:join (format-patterns patterns) "\n "))
          (data   (list mod name arity patterns* doc)))
     (lfe_io:format format data))))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun binary->term (chunk)
  (try
    (case (binary_to_term chunk)
      (ldoc (when (=:= (element 1 ldoc) 'lfe_docs_v1)) `#(ok ,ldoc))
      (_other                                           #(error badarg)))
    (catch
      (`#(error  ,reason ,_)  `#(error ,reason))
      (`#(,error ,reason ,_) `#(,error ,reason)))))

(defun exported?
  ([(match-doc exported 'true)] 'true)
  ([_]                          'false))

(defun found (mod docs)
  (flet ((doc->tuple-string
          ([(match-doc name name arity arity)]
           `#(,mod ,name ,arity))))
    (lfe_io:format "Found: ~p\n" `[,(lists:map #'doc->tuple-string/1 docs)])))

(defun source (mod)
  (try
    (let* ((compile-info (call mod 'module_info 'compile))
           (source       (proplists:get_value 'source compile-info ""))
           (`#(ok ,cwd)  (file:get_cwd)))
      (case (drop-prefix cwd source)
        (abspath (when (=:= abspath source)) abspath)
        (relpath                             (cons #\. relpath))))
    (catch
      (_error ""))))

(defun drop-prefix
  ([`(,x . ,xs) `(,y . ,ys)] (when (=:= x y))
   (drop-prefix xs ys))
  ([_xs ys]
   ys))

(defun format-patterns (patterns)
  (lists:map
    (lambda (pattern)
      (re:replace (lfe_io_pretty:term pattern) "comma " ". ,"
                  '[global #(return list)]))
    patterns))
