(ns detritus.text
  (:require [detritus.macros :refer [-<>]])
  (:require [clojure.string :as c.s]))

(defn words [text]
  (c.s/split text #"\s+"))

(defn lines [text]
  (c.s/split text #"(\n\r?)+"))

(defn trailing-newline?
  [text]
  {:pre [(string? text)]}
  (= \newline (last text)))

(defn ensure-trailing-newline
  [text]
  (if-not (trailing-newline? text)
    (str text \newline)
    text))

(defn prefix-lines [prefix text]
  (-<> (lines text)
       (map str (repeat prefix) <>)
       (interpose \newline <>)
       (apply str <>)))

(defn wrap-lines
  "(λ Num → String) → String

  Wraps the input text to the specified column width by words,
  returning the wrapped text as a single string."
  [size text]
  (loop [left  size
         line  []
         lines []
         words (words text)]
    (if-let [word (first words)]
      (let [wlen    (count word)
            spacing (if (== left size) "" " ")
            alen    (+ (count spacing) wlen)]
        (if (<= alen left)
          (recur (- left alen)
                 (conj line spacing word)
                 lines
                 (next words))
          (recur (- size wlen)
                 [word]
                 (conj lines (apply str line))
                 (next words))))
      (when (seq line)
        (conj lines (apply str line))))))


;; hiccup inspired plain text API
;;--------------------------------------------------------------------

(defn render-dispatch [f]
  (cond (vector? f)
        ,,(first f)

        (seq? f)
        ,,:seq

        true
        ,,:str))

(defmulti render
  render-dispatch)

(defmethod render :seq
  [forms]
  (->> forms
       (map render)
       (apply str)))

(defmethod render :str
  [& xs]
  (->> xs
       (map str)
       (apply str)))

(defmethod render :indent
  [[_ {width :width
       :or {width 2}
       :as opts} & text]]
  (->> text
       (map render)
       (apply str)
       (prefix-lines (apply str (repeat width \space)))))

(defmethod render :prefix
  [[_ {prefix :prefix :as opts} & text]]
  (->> text
       (prefix-lines prefix)))

(defmethod render :wrap
  [[_ {width :width
       :or {width 80}
       :as opts}
    & text]]
  (-<> text
       (map render <>)
       (apply str <>)
       (c.s/replace <> #"\n" "")
       (wrap-lines width <>)
       (interpose "\n" <>)
       (apply str <>)))

(defmethod render :heading
  [[_ opts & text]]
  (str (apply str text) "\n"
       (str (apply str (repeat 80 \-)) \newline)))

(defmethod render :code
  [[_ {lang :lang :as opts} & text]]
  (-<> text
       (map render <>)
       (apply str <>)
       (str "```" lang \newline
            <> \newline
            "```" \newline)
       (ensure-trailing-newline <>)))

(defmethod render :block
  [[_ opts & text]]
  (-<> text
       (map render <>)
       (apply str <>)
       (str <> \newline \newline)))

;; API to try and convert plain text into the above
;;--------------------------------------------------------------------

(defn text->indent
  [text]
  (let [indent (re-find #"\s\s+" text)]
    (if indent
      [:indent {:width (count indent)}
       (c.s/replace text (re-pattern indent) " ")]
      [:str {} text])))

(defn text->paragraph
  [text]
  (let [[op w text] (text->indent text)
        text        (c.s/replace text #"\n\r?" " ")]
    [:block {}
     (case op
       (:indent)
       ,,[:indent w
          [:wrap {} text]]

       (:str)
       ,,[:wrap {} text])]))

(defn text->paragraphs
  "(λ String) → (Seq String)

  Walks the input string, attempting to detect breaks between
  paragraphs and returning a sequence of strings representing
  individual paragraphs."
  [text]
  (map text->paragraph
       (c.s/split text #"\n\r?\s*\n\r?")))

;; API for manipulating text ASTs
;;--------------------------------------------------------------------

(defn unindent
  "Helper which attempts to rewrite a tree to remove `indent` levels
  of indentation. Note that negative indentation is not supported."
  [[op {width :width :as meta} & children :as text] indent]
  (if (and (= op :indent) width)
    (if (>= indent width)
      children
      `[:indent {:width ~(- width indent)} ~@children])
    text))

(defn lift-indentation
  "Helper which attempts to rewrite a given node so that it reflects
  the indentation of its children.

  Ex. [:block {} [:indent {} \"foo\"]] -> [:indent {} [:block {} \"foo\"]]"
  [[op meta & children :as text]]
  (let [children     (map #(if (vector? %1)
                             (lift-indentation %1)
                             %1)
                          children)
        indentations (->> children
                          (map (comp :width second))
                          (filter identity))]
    (if-not (empty? indentations)
      (let [mindent (apply min indentations)]
        `[:indent {:width ~mindent}
          ~@(map #(unindent %1 mindent) children)])
      text)))

(defn ensure-indent
  "Helper which ensures that the argument text is indented at least by
  width. Attempts to give a minimal indentation rather than simply
  indenting the text to width."
  [text width]
  (-> text
      lift-indentation
      (assoc-in [1 :width] width)))
