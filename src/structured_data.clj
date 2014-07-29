(ns structured-data
  (:require [clojure.math.numeric-tower :as math]))

(defn do-a-thing [x]
  (let [squared (+ x x)]
    (double (math/expt squared squared))))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z]]
  (+ x z))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[left1 left2] [right1 right2]] [point1 point2]]
  (and
   (<= left1 point1 right1)
   (<= left2 point2 right2)
    ))

(defn contains-rectangle? [outer [inner1 inner2]]
  (and
    (contains-point? outer inner1)
    (contains-point? outer inner2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [author-array (:authors book)]
    (assoc book :authors (conj author-array new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds #(get % 1)]
    (map seconds collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or 
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))    

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (let [new-authors (:authors book)]
    (assoc book :authors (set new-authors))))

(defn has-author? [book author]
  (let [auth-list (:authors book)] 
    (contains? auth-list author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (str " (" (:birth-year author) " - " (:death-year author)")")]
    (str name (if (:birth-year author) years))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string-test [books]
  (let [book-line (if (empty? books) "No books"
                      (str (count books) " " "book" (if (> (count books) 1) "s" "") "." ))]
    (str book-line " " (apply book->string books) "." )))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (str (count books) " " "book" (if (> (count books) 1) "s" "") "." " " 
     (doall (map book->string books)) ".")))


(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
