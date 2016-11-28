;;; =============================================================== [ ldoc.lfe ]
;;; Copyright (c) 2016 Eric Bailey
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;
;;; Module  : ldoc
;;; Author  : Eric Bailey
;;; Purpose : EDoc-compatible LFE documentation generator.
;;; ==================================================================== [ EOH ]
(defmodule ldoc
  "EDoc-compatible LFE documentation generator."
  (author "Eric Bailey")
  (copyright "2016 Eric Bailey")
  ;; EDoc-compatible API
  (export (get-doc 1) (get-doc 2) (get-doc 3)))

;;; ==================================================== [ EDoc-compatible API ]

;; @equiv (get-doc file)
(defun get-doc (file)
  "Equivalent to `({@link 'get-doc'/2} file [])'."
  (get-doc file []))

(defun get-doc (file opts)
  "TODO: write docstring"
  (let ((env (edoc_lib:get_doc_env opts)))
    (get-doc file env opts)))

(defun get-doc (file env opts)
  "TODO: write docstring"
  (ldoc-extract:source file env opts))

;;; ==================================================================== [ EOF ]
