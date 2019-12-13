(map! (:localleader
          (:map (clojure-mode-map clojurescript-mode-map)
            (:prefix ("a" . "add")
              "i" #'cljr-add-import-to-ns
              "p" #'cljr-add-project-dependency
              "r" #'cljr-add-require-to-ns))))
