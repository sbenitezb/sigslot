(asdf:load-system :staple-markdown)

(defclass my-page (staple:simple-page) ())

(defmethod staple:page-type ((system (eql (asdf:find-system :sigslot))))
  'my-page)

(defmethod staple:packages ((system (eql (asdf:find-system :sigslot))))
  (list (find-package :sigslot)))

(defmethod staple:format-documentation ((docstring string) (page my-page))
  (let ((*package* (first (staple:packages page))))
    (staple:markup-code-snippets-ignoring-errors
     (staple:compile-source docstring :markdown))))

(defmethod staple:documents ((system (eql (asdf:find-system :sigslot))))
  (list (asdf:system-relative-pathname system "README.md")))

(defmethod staple:subsystems ((system (eql (asdf:find-system :sigslot))))
  (list (asdf:find-system :sigslot)))
