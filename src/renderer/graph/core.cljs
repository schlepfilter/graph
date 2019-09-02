(ns graph.core
  (:require [cljs.pprint :as pprint]
            [cljs.tools.reader.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            ace
            ace-editor
            [aid.core :as aid]
            [cats.core :as m]
            [clojure.data.avl :as avl]
            [clojure.math.combinatorics :as combo]
            [cljs-node-io.core :refer [slurp spit]]
            [cljs-node-io.file :as file]
            [cljs-node-io.fs :as fs]
            cljsjs.mousetrap
            [com.rpl.specter :as s]
            [cuerdas.core :as cuerdas]
            [frp.clojure.core :as core]
            [frp.core :as frp]
            katex
            [loom.graph :as graph]
            [loom.derived :as derived]
            measure
            [oops.core :refer [oget+ oset!]]
            [reagent.core :as r]
            [thi.ng.geom.line :as line]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.core :as geom]
            [graph.helpers :as helpers]
            [graph.loom :as loom]
            [graph.parse.core :as parse])
  (:require-macros [graph.core :refer [defc]]))

(def os
  (js/require "os"))

(def path
  (js/require "path"))

(def electron
  (js/require "electron"))

(frp/defe source-append-move
          source-dimension-register
          source-directory-path
          source-edge-register
          source-in
          source-line-segment
          source-loaded-history
          source-dollar-move
          source-nearest-move
          source-node-register
          source-position
          source-scroll-x
          source-scroll-y
          source-transform-edge-action
          source-undo-redo
          append
          blockwise-visual-toggle
          down
          up
          left
          right
          carrot
          dollar
          word
          back
          delete
          yank
          paste
          dom
          escape
          insert-normal
          insert-insert
          insert-append
          command
          editor-keydown
          editor-keyup
          command-keydown
          insert-typing
          command-typing
          bounds
          implication
          submission
          completion
          undo
          redo
          close)

(def home
  (.homedir os))

(def config-path
  (path.join home (str "." helpers/app-name "rc")))

(def config-commands
  (aid/casep config-path
    fs/fexists? (->> config-path
                     slurp
                     str/split-lines
                     (remove empty?)
                     (map (partial str ":")))
    []))

(def token
  (->> (m/<> (parse/none-of " \\") (parse/string "\\ "))
       parse/some
       (m/<$> (partial apply str))))

(def delimiter
  (parse/some (parse/= \ )))

(def argument
  ((aid/lift-a (comp last
                     vector))
    delimiter
    token))

(def start
  ((aid/lift-a (partial apply str))
    (parse/= \:)
    token))

(def command-parser
  (->> argument
       parse/many
       ((aid/lift-a cons) start)))

(def parse-command
  (comp (aid/if-then-else empty?
                          identity
                          first)
        (partial parse/parse command-parser)))

(def default-directory-path
  (path.join home "Documents"))

(def directory-path-behavior
  (frp/stepper default-directory-path source-directory-path))

(def get-potential-path
  #(->>
     directory-path-behavior
     (frp/snapshot
       (->> submission
            (m/<$> parse-command)
            (core/filter (aid/build and
                                    (comp (partial = 2)
                                          count)
                                    (comp (partial (aid/flip str/starts-with?)
                                                   (str ":" %))
                                          first)))
            (m/<$> second)))
     (m/<$> (comp (aid/if-then-else (comp fs/absolute?
                                          last)
                                    last
                                    (partial apply path.join))
                  reverse))
     core/dedupe))

(def edn?
  #(try (do (edn/read-string %)
            true)
        ;TODO limit the error
        (catch js/Error _
          false)))

(def valid-file?
  ;TODO validate the keys and values
  (comp edn?
        slurp))

(def potential-file-path
  (get-potential-path "e"))

(def current-file-path-event
  (core/filter (aid/build or
                          (complement fs/fexists?)
                          (aid/build and
                                     fs/fexists?
                                     valid-file?))
               potential-file-path))

(def sink-directory-path
  (->> "cd"
       get-potential-path
       (core/filter fs/fexists?)
       (m/<> (m/<$> fs/dirname current-file-path-event))))

(def opened
  (->> current-file-path-event
       (m/<$> (complement empty?))
       (frp/stepper false)))

(def initial-cursor
  0)

(defn get-cursor-event
  [plus minus move]
  (->> (m/<> (aid/<$ (aid/if-then pos?
                                  dec)
                     minus)
             (aid/<$ inc plus)
             (m/<$> constantly move))
       (frp/accum initial-cursor)))

(def get-cursor-behavior
  (partial frp/stepper initial-cursor))

(def cursor-x-event
  (->> source-position
       (m/<$> first)
       (m/<> (aid/<$ initial-cursor carrot)
             (m/<$> :x source-undo-redo)
             source-dollar-move
             (m/<$> first source-nearest-move)
             source-append-move)
       (get-cursor-event right left)))

(def cursor-y-event
  (->> source-position
       (m/<$> last)
       (m/<> (m/<$> :y source-undo-redo)
             (m/<$> last source-nearest-move))
       (get-cursor-event down up)))

(def cursor-x-behavior
  (get-cursor-behavior cursor-x-event))

(def cursor-y-behavior
  (get-cursor-behavior cursor-y-event))

(def initial-canonical
  {})

(def reverse-sorted-map
  (partial avl/sorted-map-by (comp (partial apply compare)
                                   (partial map (comp vec
                                                      reverse))
                                   vector)))

(def initial-node
  ;TODO add :vertical
  {:canonical initial-canonical
   :id        (reverse-sorted-map)})

(def initial-edge
  (loom/digraph))

(def exit?
  (partial = "Escape"))

(def command-exit
  (->> command-keydown
       (core/filter exit?)
       (m/<> submission)))

(def llast
  (comp last
        last))

(def normal
  (->> (m/<> (aid/<$ [""] insert-normal)
             (aid/<$ ["INSERT"] (m/<> insert-insert source-append-move))
             editor-keydown)
       (core/partition 2 1)
       (core/filter (aid/build and
                               (comp (partial = "")
                                     ffirst)
                               (comp exit?
                                     llast)))
       (m/<> command-exit escape)))

(def undo-size
  100)

(def align
  (partial (aid/flip str/join) ["\\begin{aligned}" "\\end{aligned}"]))

(def get-error
  #(try (do (-> %
                align
                js/katex.renderToString)
            "")
        (catch js/katex.ParseError error
          (str error))))

(def valid-expression?
  (comp empty?
        get-error))

(def valid-expression
  (core/filter valid-expression? insert-typing))

(def typed
  (->> (m/<> cursor-x-event cursor-y-event)
       (aid/<$ false)
       (m/<> (aid/<$ true valid-expression))
       (frp/stepper false)))

(defn get-none-value
  [s x]
  (aid/casep s
    empty? s/NONE
    x))

(defn get-uuid-keyword
  []
  (-> (random-uuid)
      str
      keyword))

(defn get-set-node-action**
  [s x y]
  #(let [id (-> %
                :id
                (get [x
                      y]
                     (get-uuid-keyword)))]
     (->> %
          (s/setval [:canonical
                     id]
                    (get-none-value s {:value s
                                       :x     x
                                       :y     y}))
          (s/setval [:id
                     (s/keypath [x
                                 y])]
                    (get-none-value s id)))))

(def get-set-action*
  (comp ((aid/curry 3 s/transform*) :node)
        get-set-node-action**))

(def marker-size
  8)

(def font-size
  (* 2 marker-size))

(def cursor-size
  (* font-size 3))

(def get-x-cursor-pixel
  (comp (partial + (/ marker-size 2))
        (partial * cursor-size)))

(def blockwise-visual-mode
  (->> (m/<> (aid/<$ not blockwise-visual-toggle)
             (aid/<$ (constantly false) (m/<> delete yank escape undo)))
       (frp/accum false)
       (frp/stepper false)))

(def node-placeholder
  [js/Number.POSITIVE_INFINITY js/Number.POSITIVE_INFINITY])

(def blockwise-visual-node
  (->> (frp/snapshot blockwise-visual-toggle
                     cursor-x-behavior
                     cursor-y-behavior)
       (m/<$> rest)
       (frp/stepper node-placeholder)))

(aid/defcurried between?
  [x y z]
  (<= (min x y) z (max x y)))

(def flast
  (comp last
        first))

(defn make-between?*
  [mode a0 a1]
  (between? (if mode
              a0
              a1)
            a1))

(defn make-in?
  [mode x0 x1 y0 y1]
  (aid/build and
             (comp (make-between?* mode x0 x1)
                   ffirst)
             (comp (make-between?* mode y0 y1)
                   flast)))

(defn get-id-path
  [& more]
  [:node
   :id
   s/ALL
   (->> more
        (apply make-in?)
        s/pred)
   s/LAST])

(aid/defcurried select-ids
  [mode x0 x1 y0 y1 m]
  (s/select (get-id-path mode x0 x1 y0 y1) m))

(def distance
  (comp Math/abs
        -))

(def get-size*
  (comp (partial * cursor-size)
        inc
        distance))

(defn get-size
  [mode a0 a1]
  (get-size* (if mode
               a0
               a1)
             a1))

(defn make-delete-nodes
  [mode x0 x1 y0 y1]
  #(s/setval
     (s/multi-path (get-id-path mode x0 x1 y0 y1)
                   [:node
                    :canonical
                    (->> %
                         (select-ids mode x0 x1 y0 y1)
                         (apply (comp (aid/if-then-else empty?
                                                        (constantly s/NONE)
                                                        (partial apply
                                                                 s/multi-path))
                                      vector)))])
     s/NONE
     %))

(defn get-minimum
  [mode a0 a1]
  (if mode
    (min a0 a1)
    a1))

(defn get-delete-action*
  [mode [x0 y0] x1 y1 line-segment]
  (aid/if-then-else
    (comp empty?
          (select-ids mode x0 x1 y0 y1))
    (partial
      s/transform*
      :edge
      (partial
        (aid/flip graph/remove-edges*)
        (->> line-segment
             (filter (comp (partial geom/intersect-line
                                    (rect/rect (-> mode
                                                   (get-minimum x0 x1)
                                                   get-x-cursor-pixel)
                                               (-> mode
                                                   (get-minimum y0 y1)
                                                   (* cursor-size))
                                               (get-size mode x0 x1)
                                               (get-size mode y0 y1)))
                           val))
             (map first))))
    (comp (make-delete-nodes mode x0 x1 y0 y1)
          (aid/transfer* :edge
                         (aid/build (partial apply graph/remove-nodes)
                                    :edge
                                    (select-ids mode x0 x1 y0 y1))))))

(defn offset-paste
  [k a register]
  (s/transform [s/MAP-VALS k] (partial + a) register))

(def get-coordinate-id
  (comp (partial apply
                 reverse-sorted-map)
        (partial mapcat (aid/build vector
                                   (comp (juxt :x :y)
                                         last)
                                   first))))

(def augment
  (comp (partial zipmap [:canonical :id])
        (juxt identity
              get-coordinate-id)))

(defn deep-merge-with
  [f & more]
  (aid/if-then-else (partial every? map?)
                    (partial apply
                             merge-with
                             (partial deep-merge-with f))
                    (partial apply f)
                    more))

(def deep-merge
  (partial deep-merge-with (comp last
                                 vector)))

(defn make-remap
  [m]
  #(get m % %))

(defn get-paste-action*
  [dimension node-register edge-register x y]
  (let [remapping (s/transform s/MAP-VALS
                               (fn [_]
                                 (get-uuid-keyword))
                               node-register)]
    (comp (aid/build (partial s/transform* :edge)
                     (comp (aid/flip (aid/curry 2 graph/add-edges*))
                           graph/edges
                           (partial graph/subgraph
                                    (derived/mapped-by (make-remap remapping)
                                                       edge-register))
                           keys
                           :canonical
                           :node)
                     identity)
          (partial s/transform*
                   :node
                   (partial deep-merge
                            (->> node-register
                                 (s/transform s/MAP-KEYS (make-remap remapping))
                                 (offset-paste :x x)
                                 (offset-paste :y y)
                                 augment)))
          (make-delete-nodes true
                             x
                             (-> dimension
                                 first
                                 (+ x))
                             y
                             (-> dimension
                                 last
                                 (+ y))))))

(def initial-dimension
  [0 0])

(def graph-action
  (->> (m/<> (frp/snapshot (->> (frp/snapshot normal typed)
                                (core/filter last)
                                (m/<$> first))
                           ((aid/lift-a get-set-action*)
                             (frp/stepper "" valid-expression)
                             cursor-x-behavior
                             cursor-y-behavior))
             (frp/snapshot delete
                           ((aid/lift-a get-delete-action*)
                             blockwise-visual-mode
                             blockwise-visual-node
                             cursor-x-behavior
                             cursor-y-behavior
                             (frp/stepper {} source-line-segment)))
             (frp/snapshot paste
                           ((aid/lift-a get-paste-action*)
                             (frp/stepper initial-dimension
                                          source-dimension-register)
                             (frp/stepper {} source-node-register)
                             (frp/stepper initial-edge source-edge-register)
                             cursor-x-behavior
                             cursor-y-behavior)))
       (m/<$> last)
       (m/<> source-transform-edge-action)))

(def action
  (m/<$> (comp #(aid/if-else (comp (aid/build =
                                              %
                                              identity)
                                   ffirst)
                             (comp (partial s/setval* s/LAST [])
                                   (partial s/transform*
                                            s/FIRST
                                            (partial take undo-size))
                                   (aid/transfer* [s/FIRST s/BEFORE-ELEM]
                                                  (comp %
                                                        ffirst))))
               (fn [[f x y]]
                 (comp (partial s/setval* :y y)
                       (partial s/setval* :x x)
                       f)))
         (frp/snapshot graph-action cursor-x-behavior cursor-y-behavior)))

(def multiton?
  (comp (partial < 1)
        count))

(def lfirst
  (comp first
        last))

(def get-history
  (comp (partial (aid/flip vector) [])
        vector))

(def initial-history
  (get-history {:node initial-node
                :edge initial-edge}))

(def reset
  (m/<$> constantly source-loaded-history))

(def valid-undo?
  (comp multiton?
        first))

(def valid-redo?
  (comp not-empty
        last))

(def ongoing-history-event
  (->> action
       (m/<> (aid/<$ (aid/if-then valid-undo?
                                  (comp (partial s/transform*
                                                 s/FIRST
                                                 rest)
                                        (aid/transfer* [s/LAST
                                                        s/BEFORE-ELEM]
                                                       ffirst)))
                     undo)
             (aid/<$ (aid/if-then valid-redo?
                                  (comp (partial s/transform* s/LAST rest)
                                        (aid/transfer* [s/FIRST
                                                        s/BEFORE-ELEM]
                                                       lfirst)))
                     redo)
             reset)
       (frp/accum initial-history)))

(def content
  (m/<$> ffirst ongoing-history-event))

(def ongoing-history-behavior
  (frp/stepper initial-history ongoing-history-event))

(defn get-valid
  [f g e]
  (->> ongoing-history-behavior
       (frp/snapshot e)
       (m/<$> last)
       (core/filter f)
       (m/<$> g)))

(def sink-undo-redo
  (m/<> (get-valid valid-undo? ffirst undo)
        (get-valid valid-redo? lfirst redo)))

(def edge-event
  (m/<$> :edge content))

(def edge-behavior
  (frp/stepper initial-edge edge-event))

(def node-event
  (->> (frp/snapshot valid-expression cursor-x-behavior cursor-y-behavior)
       (m/<$> (partial apply get-set-node-action**))
       (m/<> (m/<$> (comp constantly
                          :node)
                    content))
       (frp/accum initial-node)))

(def node-behavior
  (frp/stepper initial-node node-event))

(def canonical
  (m/<$> :canonical node-behavior))

(def id
  (m/<$> :id node-behavior))

(defn make-offset-register
  [mode k a0 a1]
  (aid/build (partial s/transform* [s/MAP-VALS k])
             (comp (aid/flip (aid/curry 2 -))
                   (partial apply
                            min
                            (if mode
                              a0
                              js/Number.POSITIVE_INFINITY)
                            a1)
                   (partial map k)
                   vals)
             identity))

(def sink-node-register
  (m/<$> (fn [[_ m mode [x0 y0] x1 y1]]
           (->> m
                (s/setval [s/ALL
                           (-> (aid/build and
                                          (comp (make-between?* mode x0 x1)
                                                :x
                                                last)
                                          (comp (make-between?* mode y0 y1)
                                                :y
                                                last))
                               complement
                               s/pred)]
                          s/NONE)
                ((make-offset-register mode :x x0 x1))
                ((make-offset-register mode :y y0 y1))))
         (frp/snapshot (m/<> delete yank)
                       canonical
                       blockwise-visual-mode
                       blockwise-visual-node
                       cursor-x-behavior
                       cursor-y-behavior)))

(def sink-dimension-register
  (m/<$> (fn [[_ mode a & b]]
           (if mode
             (map distance a b)
             initial-dimension))
         (frp/snapshot delete
                       blockwise-visual-mode
                       blockwise-visual-node
                       cursor-x-behavior
                       cursor-y-behavior)))

(def neighbors
  (aid/build set/union
             loom/predecessors
             loom/successors))

(def get-nodes
  (aid/build conj
             neighbors
             (comp last
                   vector)))

(def sink-edge-register
  (m/<$> (fn [[node-register edge]]
           (->> node-register
                keys
                (mapcat (partial get-nodes edge))
                (graph/subgraph edge)))
         (frp/snapshot sink-node-register edge-behavior)))

(aid/defcurried extract-insert
  [n coll]
  (->> coll
       (s/setval s/FIRST s/NONE)
       (s/setval (s/before-index n) (first coll))))

(defn zip-entities
  [f es bs]
  (->> es
       (map-indexed (fn [n e]
                      (->> bs
                           (s/setval (s/nthpath n) s/NONE)
                           (apply frp/snapshot e)
                           (m/<$> (comp f
                                        (extract-insert n))))))
       (apply m/<>)))

(def insert-text
  (->> insert-typing
       (m/<> (zip-entities (fn [[x y m]]
                             (if-let [id* ((:id m) [x y])]
                               (-> m
                                   :canonical
                                   id*
                                   :value)
                               ""))
                           [cursor-x-event cursor-y-event node-event]
                           [cursor-x-behavior
                            cursor-y-behavior
                            node-behavior]))
       (frp/stepper "")))

(def edge-node-event
  (->> (frp/snapshot implication
                     insert-text
                     cursor-x-behavior
                     cursor-y-behavior
                     id)
       (core/remove (comp empty?
                          second))
       (m/<$> (comp (aid/build aid/funcall
                               last
                               (partial take 2))
                    (partial drop 2)))))

(def edge-node-behavior
  (frp/stepper node-placeholder edge-node-event))

(defn add-scroll
  [k0 k1 scroll bound]
  (s/transform (s/multi-path k0 k1) (partial + scroll) bound))

(def valid-bound-event
  (->> (frp/snapshot bounds
                     (frp/stepper 0 source-scroll-x)
                     (frp/stepper 0 source-scroll-y))
       (m/<$> (comp (aid/flip (aid/curry 2 merge))
                    (aid/build hash-map
                               :id
                               identity)
                    (fn [[bound scroll-x scroll-y]]
                      (->> bound
                           (add-scroll :left :right scroll-x)
                           (add-scroll :top :bottom scroll-y)))))
       (m/<> (m/<$> (comp (aid/curry 2 s/select-one*)
                          s/submap
                          keys
                          :canonical)
                    node-event))
       (frp/accum {})))

(def valid-bound-behavior
  (frp/stepper {} valid-bound-event))

(defn nearest
  ([coll test x]
   (avl/nearest coll test x))
  ([coll test x default]
   (aid/if-then nil?
                (constantly default)
                (avl/nearest coll test x))))

(def get-nearest-key
  (comp first
        (partial apply nearest)
        (partial s/transform* s/LAST vector)
        vector))

(aid/defcurried get-end
  [f y id*]
  (aid/if-then-else (comp (partial = y)
                          last)
                    f
                    (constantly 0)
                    (get-nearest-key id* < [0 (inc y)] [0 y])))

(def sink-dollar-move
  (m/<$> (comp (partial apply (get-end first))
               rest)
         (frp/snapshot dollar cursor-y-behavior id)))

(def sink-append-move
  (m/<$> (fn [[_ y node bound]]
           (->> node
                :id
                (get-end (aid/build +
                                    (comp inc
                                          (partial (aid/flip quot) cursor-size)
                                          :width
                                          bound
                                          (:id node))
                                    first)
                         y)))
         (frp/snapshot append
                       cursor-y-behavior
                       node-behavior
                       valid-bound-behavior)))

(def sink-nearest-move
  (m/<$> (fn [[f x y node]]
           (get-nearest-key (:id node) f [x y] [x y]))
         (frp/snapshot (m/<> (aid/<$ > word)
                             (aid/<$ < back))
                       cursor-x-behavior
                       cursor-y-behavior
                       node-behavior)))

(def make-directional
  #(comp (partial apply =)
         (partial map %)))

(def horizontal?
  (make-directional :top))

(def vertical?
  (make-directional :left))

(def oblique?
  (complement (aid/build or
                         horizontal?
                         vertical?)))

(def mean
  (aid/build /
             (partial apply +)
             count))

(def get-center
  (comp (partial map mean)
        (juxt (juxt :left
                    :right)
              (juxt :top
                    :bottom))))

(def shrink
  (partial (aid/flip -) (* 2 font-size)))

(def get-left-top
  (juxt :left :top))

(aid/defcurried get-intersection-line-segment
  [f [out in]]
  [(f out)
   (geom/intersect-line ((aid/build rect/rect
                                    :left
                                    :top
                                    :width
                                    :height)
                          in)
                        (line/line2 (f out) (f in)))])

(def get-left-top-line-segment
  (get-intersection-line-segment get-left-top))

(def get-direction
  (partial apply map (comp neg?
                           -)))

(def get-left-top-direction
  (comp get-direction
        (partial map get-left-top)))

(def get-corner-line-segment
  (aid/build (partial map aid/funcall)
             (comp (partial apply map juxt)
                   (partial map (aid/flip aid/funcall) [[:right :left]
                                                        [:bottom :top]])
                   (partial map #(if %
                                   identity
                                   reverse))
                   get-left-top-direction)
             identity))

(def corners?
  (aid/build =
             get-left-top-direction
             (comp get-direction
                   get-corner-line-segment)))

(def get-line-segment
  (aid/if-then-else
    oblique?
    (aid/if-then-else corners?
                      get-corner-line-segment
                      (get-intersection-line-segment get-center))
    (aid/if-then-else (comp horizontal?)
                      (comp (partial s/transform*
                                     [s/ALL s/LAST]
                                     (partial + font-size))
                            get-left-top-line-segment)
                      get-left-top-line-segment)))

(def correspondence
  ((aid/lift-a (fn [m coll]
                 (->> coll
                      (map (aid/if-then-else (partial every? m)
                                             (aid/build hash-map
                                                        identity
                                                        (comp get-line-segment
                                                              (partial map m)))
                                             (constantly {})))
                      (apply merge {}))))
    (m/<$> (partial s/transform*
                    s/MAP-VALS
                    (comp (partial s/transform*
                                   (s/multi-path :top
                                                 :bottom)
                                   (partial + marker-size))
                          (partial s/transform*
                                   (s/multi-path :bottom
                                                 :height)
                                   shrink)))
           valid-bound-behavior)
    (->> edge-event
         (m/<$> graph/edges)
         (frp/stepper []))))

(def sink-line-segment
  (m/<$> (partial s/transform* s/MAP-VALS line/line2) correspondence))

(def error
  (m/<$> get-error insert-text))

(def command-text
  (->> command-exit
       (aid/<$ "")
       (m/<> command-typing)
       (frp/stepper "")))

(def mode-event
  (m/<> (aid/<$ [:normal] normal)
        (aid/<$ [:insert
                 :insert]
                (m/<> insert-normal insert-insert source-append-move))
        (aid/<$ [:insert :append] (m/<> insert-append))
        (aid/<$ [:command] command)))

(def mode-behavior
  (frp/stepper [:normal] mode-event))

(def outer-mode
  (m/<$> first mode-behavior))

(def edge-mode
  (->> source-in
       (m/<> mode-event ongoing-history-event)
       (aid/<$ false)
       (m/<> (aid/<$ true edge-node-event))
       (frp/stepper false)))

(def sink-in
  (->> edge-mode
       (frp/snapshot (core/dedupe edge-node-event))
       (core/filter last)
       (m/<$> first)))

(def additional-edge
  (->> edge-node-event
       (frp/stepper node-placeholder)
       (frp/snapshot sink-in)
       (m/<$> reverse)))

(def sink-transform-edge-action
  (m/<$> #(partial s/transform* :edge (partial (aid/flip graph/add-edges) %))
         additional-edge))

(def editor-command
  (->> editor-keyup
       (core/filter (partial = ":"))
       (aid/<$ false)
       (m/<> (m/<$> (comp (partial = ":")
                          last)
                    editor-keydown))
       (frp/stepper false)))

(def file-path-placeholder
  "")

(def current-file-path-behavior
  (frp/stepper file-path-placeholder current-file-path-event))

(def modification
  (->> (frp/snapshot (m/<> ongoing-history-event
                           (m/<$> last
                                  (frp/snapshot (m/<> current-file-path-event
                                                      close)
                                                ongoing-history-behavior)))
                     current-file-path-behavior)
       (m/<$> (comp (partial s/transform*
                             s/LAST
                             (comp #(dissoc % :x :y)
                                   (partial s/transform*
                                            :edge
                                            graph/edges)
                                   (partial s/transform*
                                            :node
                                            :canonical)
                                   ffirst))
                    reverse))
       ;TODO apply point-free style
       (core/remove (fn [[path* m]]
                      (or (empty? path*)
                          (and (fs/fexists? path*)
                               (-> path*
                                   slurp
                                   edn/read-string
                                   (= m))))))))

(def info-name
  (str helpers/app-name "info"))

(def option-parser
  ((aid/lift-a (comp last
                     vector))
    (parse/string info-name)
    ;TODO implement "="
    (parse/string "+=")
    (parse/= \n)
    token))

(def info-path
  (->> directory-path-behavior
       (frp/snapshot (->> submission
                          (m/<$> parse-command)
                          (core/filter (comp (partial = ":set")
                                             first))
                          (m/<$> (comp (partial parse/parse option-parser)
                                       last))
                          (core/remove empty?)
                          (m/<$> first)))
       (m/<$> (comp (partial apply path.join)
                    reverse))))

(def default-info-path
  (->> info-name
       (str ".")
       (path.join home)))

(def config-info-path
  (->> info-path
       (frp/stepper default-info-path)
       (frp/snapshot completion)
       (core/take 1)
       (m/<$> last)))

(def initial-jumplist
  [])

(def config-info
  (m/<$> (aid/if-then-else fs/fexists?
                           (comp edn/read-string
                                 slurp)
                           (constantly {:jumplist initial-jumplist}))
         config-info-path))

(def previous-path-position
  (->> (frp/snapshot (m/<> current-file-path-event
                           close)
                     current-file-path-behavior
                     cursor-x-behavior
                     cursor-y-behavior)
       (m/<$> rest)
       (core/remove (comp empty?
                          first))))

(def maximum-jumplist-size
  100)

(def jumplist
  ;TODO implement jump
  (->> previous-path-position
       (m/<$> vector)
       (m/<> (m/<$> (comp reverse
                          :jumplist)
                    config-info))
       core/concat
       (m/<$> (comp (partial take maximum-jumplist-size)
                    reverse))))

(def info
  ;TODO implement command history
  (m/<$> (partial hash-map :jumplist) jumplist))

(def info-path-info
  (->> info-path
       (frp/stepper default-info-path)
       (frp/snapshot info)
       (m/<$> reverse)))

(defn get-first
  [pred coll not-found]
  (aid/if-then-else empty?
                    (constantly not-found)
                    first
                    (filter pred coll)))

(def sink-position
  (->> jumplist
       (frp/stepper initial-jumplist)
       (frp/snapshot current-file-path-event)
       (m/<$> (partial apply (fn [path* coll]
                               (->> ["" initial-cursor initial-cursor]
                                    (get-first (comp (partial = path*)
                                                     first)
                                               coll)
                                    rest))))))

(def sink-loaded-history
  (->> (frp/snapshot current-file-path-event
                     current-file-path-behavior
                     ongoing-history-behavior)
       (m/<$> (comp (partial apply hash-map)
                    rest))
       core/merge
       (frp/stepper {})
       (frp/snapshot current-file-path-event)
       (m/<$> (fn [[k m]]
                (aid/casep k
                  fs/fexists? (->> k
                                   slurp
                                   edn/read-string
                                   (s/transform :node augment)
                                   (s/transform :edge
                                                (partial apply loom/digraph))
                                   get-history
                                   (get m k))
                  initial-history)))))

(def initial-scroll
  0)

(def initial-maximum
  0)

(def client-width
  (->> dom
       (m/<$> :client-width)
       (frp/stepper js/Number.MAX_VALUE)))

(def client-height
  (->> dom
       (m/<$> :client-height)
       (frp/stepper js/Number.MAX_VALUE)))

(def editing-bound
  (->> (frp/snapshot valid-bound-event
                     id
                     cursor-x-behavior
                     cursor-y-behavior)
       (core/partition 2 1)
       (core/filter (comp (partial apply =)
                          (partial map (partial drop 2))))
       (m/<$> last)
       (m/<> (m/<$> rest
                    (frp/snapshot insert-insert
                                  valid-bound-behavior
                                  id
                                  cursor-x-behavior
                                  cursor-y-behavior)))
       (m/<$> (fn [[m id & coordinate]]
                (aid/if-then-else id
                                  (comp m
                                        id)
                                  (constantly {})
                                  ;TODO delete vec when sorted-map supports comparing a vector and IndexedSeq
                                  (vec coordinate))))))

(def get-editing-bound
  #(m/<$> (comp (partial (aid/flip quot) cursor-size)
                (partial max initial-maximum)
                %)
          editing-bound))

(def editing-x-bound
  (get-editing-bound :right))

(def editing-y-bound
  (get-editing-bound :bottom))

(def opening
  (->> (aid/<$ true source-loaded-history)
       (m/<> (aid/<$ false (m/<> action valid-expression)))
       (frp/stepper true)))

(defn get-scroll
  [client bound offset* cursor]
  (->> client
       (frp/snapshot (->> cursor
                          (frp/stepper initial-cursor)
                          (frp/snapshot bound opening)
                          (m/<$> (fn [[bound* opening* cursor*]]
                                   (if opening*
                                     cursor*
                                     (max bound* cursor*))))
                          (m/<> cursor)))
       (core/reduce (fn [reduction [x view-size]]
                      (-> reduction
                          (max (-> x
                                   inc
                                   (* cursor-size)
                                   (- (- view-size offset*))))
                          (min (* x cursor-size))))
                    initial-scroll)
       (frp/stepper initial-scroll)))

(def sink-scroll-x
  (get-scroll client-width editing-x-bound marker-size cursor-x-event))

(def sink-scroll-y
  (get-scroll client-height editing-y-bound 0 cursor-y-event))

(def get-pixel
  (partial m/<$> (comp (partial * cursor-size)
                       inc)))

(defn get-maximum
  [client scroll bound cursor]
  ((aid/lift-a max)
    ((aid/lift-a +) client scroll)
    (get-pixel (frp/stepper initial-maximum bound))
    (get-pixel cursor)))

(def maximum-x
  (get-maximum client-width sink-scroll-x editing-x-bound cursor-x-behavior))

(def maximum-y
  (get-maximum client-height sink-scroll-y editing-y-bound cursor-y-behavior))

(aid/defcurried effect
  [f x]
  (f x)
  x)

(defn memoize-one
  [f!]
  ;TODO use core.memoize when core.memoize supports ClojureScript
  (let [state (atom {})]
    (fn [& more]
      (aid/case-eval more
        (:arguments @state) (:return @state)
        (->> more
             (apply f!)
             (effect #(reset! state {:arguments more
                                     :return    %})))))))

(def get-status-text
  #(.keyBinding.getStatusText (:editor %) (:editor %)))

(def upper?
  (aid/build =
             identity
             str/upper-case))

(def get-mathcal
  (partial (aid/flip str/join) ["\\mathcal{" "}"]))

(aid/defcurried get-command
  [state [keyboard s]]
  {:bindKey (aid/if-then upper?
                         (partial str "shift+")
                         keyboard)
   :exec    #(.insert (:editor @state)
                      (case (:math @state)
                        :backtick (aid/if-else empty?
                                               (partial str "\\")
                                               s)
                        (if (str/starts-with? keyboard "ctrl")
                          ""
                          (case (:math @state)
                            :c (get-mathcal keyboard)
                            keyboard))))
   :name    keyboard})

(def alphabets
  (mapcat (fn [[start* end]]
            (->> end
                 pprint/char-code
                 inc
                 (range (pprint/char-code start*))
                 (map char)))
          [[\A \Z]
           [\a \z]]))

(def default-keymap
  (->> ""
       repeat
       (zipmap alphabets)))

(def upper-keymap
  {"D" "Delta"
   "G" "Gamma"
   "J" "Theta"
   "L" "Lambda"
   "X" "Xi"
   "P" "Pi"
   "S" "Sigma"
   "U" "Upsilon"
   "F" "Phi"
   "Y" "Psi"
   "W" "Omega"})

(def lower-keymap
  (merge (s/transform (s/multi-path s/MAP-KEYS s/MAP-VALS)
                      str/lower-case
                      upper-keymap)
         {"a" "alpha"
          "b" "beta"
          "e" "epsilon"
          "z" "zeta"
          "h" "eta"
          "k" "kappa"
          "m" "mu"
          "n" "nu"
          "r" "rho"
          "t" "tau"
          "q" "chi"}))

(def symbol-keymap
  {"<"  "le"
   ">"  "ge"
   "~"  "tilde"
   "^"  "hat"
   "N"  "nabla"
   "I"  "infty"
   "A"  "forall"
   "E"  "exists"
   "/"  "not"
   "i"  "in"
   "*"  "times"
   "."  "cdot"
   ":"  "colon"
   "{"  "sub"
   "}"  "supset"
   "["  "sube"
   "]"  "supe"
   "0"  "empty"
   "\\" "setminus"
   "+"  "cup"
   "-"  "cap"
   "("  "langle"
   ")"  "rangle"
   ;"|" doesn't seem to get registered possibly because "|" is used as a special token in Ace
   "|"  "vee"
   "&"  "wedge"})

(def ctrl-keymap
  (s/transform s/MAP-KEYS (partial str "ctrl+") {"e" "exp"
                                                 "s" "sin"
                                                 ;"ctrl+c" doesn't seem to get registered possibly because "ctrl+c" is handled separately in Ace
                                                 "c" "cos"
                                                 "^" "sup"
                                                 "_" "inf"
                                                 "d" "det"
                                                 "l" "lim"
                                                 "t" "tan"}))

(def math-keymap
  (merge default-keymap lower-keymap upper-keymap symbol-keymap ctrl-keymap))

(defc editor
      [& _]
      (let [state (atom {:math :other})]
        (r/create-class
          {:component-did-mount
           (fn [_]
             (-> @state
                 :editor
                 .textInput.getElement
                 (.addEventListener "keydown"
                                    #(editor-keydown [(get-status-text @state)
                                                      (.-key %)])))
             (-> @state
                 :editor
                 .textInput.getElement
                 (.addEventListener "keyup"
                                    (fn [event*]
                                      (-> @state
                                          get-status-text
                                          editor-keyup)
                                      (swap! state
                                             (partial s/transform*
                                                      :math
                                                      #(case (.-key event*)
                                                         "`" :backtick
                                                         "c" (case %
                                                               :backtick :c
                                                               :other)
                                                         :other)))))))
           :component-did-update
           (fn [_]
             (if (-> @state
                     :mode
                     last
                     (= :append))
               (-> @state
                   :editor
                   (.moveCursorTo js/Number.MAX_VALUE js/Number.MAX_VALUE)))
             (if (-> @state
                     :mode
                     first
                     (= :normal))
               (-> @state
                   :editor
                   .blur)))
           :reagent-render
           (fn [mode text]
             (swap! state (partial s/setval* :mode mode))
             [:> ace-editor
              {:commands         (->> math-keymap
                                      (map (get-command state))
                                      (concat
                                        [{:bindKey "`"
                                          :exec    aid/nop
                                          :name    "backtick"}
                                         {:bindKey "c"
                                          :exec    #(.insert
                                                      (:editor @state)
                                                      (case (:math @state)
                                                        :backtick ""
                                                        :c (get-mathcal "c")
                                                        "c"))
                                          :name    "c"}]))
               :focus            (-> mode
                                     first
                                     (= :insert))
               :keyboard-handler "vim"
               :mode             "latex"
               :on-change        #(insert-typing %)
               :ref              #(if %
                                    (swap! state
                                           (partial s/setval*
                                                    :editor
                                                    (.-editor %))))
               :style            {:font-size font-size
                                  :height    "100%"
                                  :width     "100%"}
               :theme            "terminal"
               :value            text}])})))

(defn math
  [s]
  [:div
   {:dangerouslySetInnerHTML
    {:__html (js/katex.renderToString s
                                      #js {:displayMode true})}}])

(def background-color
  "black")

(def selection-color
  "grey")

(def color
  "white")

(def outline-width
  1)

(defn get-node-color
  [mode edge-node id*]
  (if (and mode
           (= edge-node id*))
    selection-color
    background-color))

(defn math-node
  [& _]
  (let [state (r/atom {:height js/Number.MAX_VALUE})]
    (fn [mode edge-node [id* {:keys [value x y]}]]
      [:g
       [:rect (merge (s/transform :height shrink @state)
                     {:fill  (get-node-color mode edge-node id*)
                      :style {:outline-color (get-node-color mode
                                                             edge-node
                                                             id*)
                              :outline-style "solid"
                              :outline-width outline-width}
                      :x     (get-x-cursor-pixel x)
                      :y     (* y cursor-size)})]
       ^{:key id*}
       [:> measure
        {:bounds    true
         :on-resize #(-> %
                         .-bounds
                         (js->clj :keywordize-keys true)
                         ((juxt (comp bounds
                                      (partial s/setval* :id id*))
                                (partial reset! state))))}
        #(r/as-element [:foreignObject {:x (get-x-cursor-pixel x)
                                        :y (* y cursor-size)}
                        [:div {:ref   (.-measureRef %)
                               :style {:display       "inline-block"
                                       :margin-bottom (- marker-size)
                                       :margin-top    (- marker-size)}}
                         [math (align value)]]])]])))

(def maximum-z-index
  ;https://stackoverflow.com/a/25461690
  2147483647)

(def get-percent
  (comp (partial (aid/flip str) "%")
        (partial * 100)))

(def left-pane
  0.5)

(def right-pane
  (- 1 left-pane))

(def message-style
  {:background-color background-color
   :border           "none"
   :bottom           0
   :color            color
   :position         "absolute"
   :z-index          maximum-z-index})

(defc input-component
      [_]
      (r/create-class
        {:component-did-update #(-> %
                                    r/dom-node
                                    .focus)
         :reagent-render       (fn [s]
                                 [:input
                                  {:on-change   #(-> %
                                                     .-target.value
                                                     command-typing)
                                   :on-key-down #(-> %
                                                     .-key
                                                     command-keydown)
                                   :style       (s/setval
                                                  :width
                                                  (get-percent left-pane)
                                                  message-style)
                                   :value       s}])}))

(defc command-component
      [mode command-text*]
      [:form {:style     {:display (case mode
                                     :command "block"
                                     "none")}
              :on-submit #(submission command-text*)}
       [input-component command-text*]])

(def memoized-keyword
  (memoize cuerdas/keyword))

(aid/defcurried convert-keys
  [ks x]
  (->> ks
       (mapcat (juxt memoized-keyword
                     #(case (-> x
                                (oget+ %)
                                goog/typeOf)
                        "function" (partial js-invoke x %)
                        (oget+ x %))))
       (apply hash-map)))

;(def convert-object
;  (aid/build convert-keys
;             object/getKeys
;             identity))
;
;(def dom*
;  (comp dom
;        convert-object
;        r/dom-node))
;
;=> @dom
;#object[TypeError TypeError: Cannot convert a Symbol value to a string]
(def dom*
  (comp dom
        (convert-keys #{"clientWidth" "clientHeight"})
        r/dom-node))

(defc nodes
      [mode edge-node canonical*]
      (->> canonical*
           (mapv (partial vector math-node mode edge-node))
           (s/setval s/BEFORE-ELEM :g)))

(def edge-component
  (comp (partial vector :line)
        ;TODO add :on-click
        (partial s/setval* :style {:marker-end   "url(#arrow)"
                                   :stroke-width 1
                                   :stroke       color})
        (partial zipmap [:x1 :y1 :x2 :y2])
        flatten
        last))

(defc edges-component
      [correspondence*]
      (->> correspondence*
           (mapv edge-component)
           (s/setval s/BEFORE-ELEM :g)))

(defc blockwise-visual-component
      [mode [x0 y0] x1 y1]
      [:rect (if mode
               {:fill   selection-color
                :height (get-size* y1 y0)
                :width  (get-size* x1 x0)
                :x      (get-x-cursor-pixel (min x0 x1))
                :y      (* (min y0 y1) cursor-size)}
               {})])

(def ref-x
  2)

(def view-box
  (->> ref-x
       (repeat 2)
       (concat (repeat 2 0))
       (str/join " ")))

(def ref-y
  1)

(def path-d
  (str/join " " ["M" 0 0 "L" ref-x ref-y "L" 0 ref-x "z"]))

(defc graph-component
      [& _]
      (let [state (atom {})]
        (r/create-class
          {:component-did-mount  (fn [this]
                                   (dom* this)
                                   (js/window.addEventListener "resize"
                                                               (fn [_]
                                                                 (dom* this))))
           :component-did-update #(-> %
                                      r/dom-node
                                      (.scrollTo (:x @state)
                                                 (:y @state)))
           :reagent-render       (fn [scroll-x*
                                      scroll-y*
                                      maximum-x
                                      maximum-y
                                      edge-mode*
                                      edge-node
                                      canonical*
                                      correspondence*
                                      blockwise-visual-mode*
                                      blockwise-visual-node*
                                      cursor-x
                                      cursor-y]
                                   (swap! state
                                          (partial (aid/flip merge)
                                                   {:x scroll-x*
                                                    :y scroll-y*}))
                                   [:div {:style {:height   "100%"
                                                  :overflow "scroll"
                                                  :width    "100%"}}
                                    [:svg {:style {:height maximum-y
                                                   :width  maximum-x}}
                                     [:marker {:id            "arrow"
                                               :marker-width  marker-size
                                               :marker-height marker-size
                                               :orient        "auto"
                                               :ref-x         ref-x
                                               :ref-y         ref-y
                                               :view-box      view-box}
                                      [:path {:d    path-d
                                              :fill color}]]
                                     [blockwise-visual-component
                                      blockwise-visual-mode*
                                      blockwise-visual-node*
                                      cursor-x
                                      cursor-y]
                                     [edges-component correspondence*]
                                     [nodes edge-mode* edge-node canonical*]
                                     [:rect
                                      {:height  cursor-size
                                       :opacity 0
                                       :style   {:outline-color  "red"
                                                 :outline-offset (-
                                                                   outline-width)
                                                 :outline-style  "solid"
                                                 :outline-width  outline-width}
                                       :width   cursor-size
                                       :x       (get-x-cursor-pixel cursor-x)
                                       :y       (* cursor-y
                                                   cursor-size)}]]])})))

(defc error-component
      [error* editor-command*]
      [:div {:style (merge message-style {:display (if (or (empty? error*)
                                                           editor-command*)
                                                     "none"
                                                     "block")
                                          :width   (get-percent right-pane)})}
       error*])

(defc app-component
      [opened* graph-view* command-view* editor-view* error-view*]
      (s/setval s/BEGINNING
                [:div {:style {:background-color background-color
                               :color            color
                               :display          "flex"
                               :height           "100%"
                               :overflow         "hidden"
                               :width            "100%"}}]
                (if opened*
                  [[:div {:style {:width (get-percent left-pane)}}
                    graph-view*
                    command-view*]
                   [:div {:style {:width (get-percent right-pane)}}
                    editor-view*
                    error-view*]]
                  [command-view*])))

(def graph-view
  ((aid/lift-a graph-component)
    sink-scroll-x
    sink-scroll-y
    maximum-x
    maximum-y
    edge-mode
    edge-node-behavior
    canonical
    correspondence
    blockwise-visual-mode
    blockwise-visual-node
    cursor-x-behavior
    cursor-y-behavior))

(def command-view
  ((aid/lift-a command-component) outer-mode command-text))

(def editor-view
  ((aid/lift-a editor) mode-behavior insert-text))

(def error-view
  ((aid/lift-a error-component) error editor-command))

(def app-view
  ((aid/lift-a app-component)
    opened
    graph-view
    command-view
    editor-view
    error-view))

;TODO don't use two events when ClojureScript supports lazy evaluation
(def loop-event
  (partial run! (partial apply frp/run)))

(loop-event {source-append-move           sink-append-move
             source-directory-path        sink-directory-path
             source-dimension-register    sink-dimension-register
             source-dollar-move           sink-dollar-move
             source-edge-register         sink-edge-register
             source-in                    sink-in
             source-line-segment          sink-line-segment
             source-loaded-history        sink-loaded-history
             source-nearest-move          sink-nearest-move
             source-node-register         sink-node-register
             source-position              sink-position
             source-scroll-x              sink-scroll-x
             source-scroll-y              sink-scroll-y
             source-transform-edge-action sink-transform-edge-action
             source-undo-redo             sink-undo-redo})

(frp/run #(oset! js/document "title" %) current-file-path-event)

(frp/run (partial (aid/flip r/render) (js/document.getElementById "app"))
         app-view)

(oset! js/window "onsubmit" #(.preventDefault %))

(defn bind
  [s e]
  (js/Mousetrap.bind s #(e)))

(def bind-keymap
  (partial run! (partial apply bind)))

(def graph-keymap
  {"$"      dollar
   ":"      command
   "\\"     implication
   "^"      carrot
   "ctrl+r" redo
   "ctrl+v" blockwise-visual-toggle
   "escape" escape
   "a"      insert-append
   "A"      append
   "b"      back
   "h"      left
   "i"      insert-insert
   "j"      down
   "k"      up
   "l"      right
   "p"      paste
   "space"  insert-normal
   "u"      undo
   "w"      word
   "x"      delete
   ;TODO replace "y" with all keys capture when mousetrap supports all keys capture
   ;https://github.com/ccampbell/mousetrap/issues/134
   "y"      yank})

(bind-keymap graph-keymap)

(.config.loadModule ace
                    "ace/keyboard/vim"
                    #(run! (partial apply (.-CodeMirror.Vim.map %))
                           (combo/cartesian-product ["jk" "kj"]
                                                    ["<Esc>"]
                                                    ["insert" "command"])))

(.ipcRenderer.on electron
                 helpers/channel
                 (comp (aid/build (partial apply aid/funcall)
                                  (comp {"close" close
                                         "open"  potential-file-path}
                                        first)
                                  rest)
                       last
                       vector))

(frp/run (partial apply spit) modification)

;TODO delete this function when cljs-node-io.fs supports mkdirs
(def mkdirs
  #(.mkdirs (file/File. %)))

(def make-+
  #(comp (juxt (comp mkdirs
                     fs/dirname
                     first)
               (partial apply %))
         vector))

(def spit+
  (make-+ spit))

(frp/run (partial apply spit+) info-path-info)

(frp/run (fn [_]
           (electron.remote.app.exit))
         close)

(frp/activate)

(run! submission config-commands)

(completion)
