;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "ref"
 (lambda ()
   (LaTeX-add-bibitems
    "nordland2023policy"
    "tsiatis2006semiparametric"
    "laan2003unified"))
 '(or :bibtex :latex))

