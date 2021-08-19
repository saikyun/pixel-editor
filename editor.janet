(import freja/events :as e)
(import freja/state)
(import freja/hiccup :as h)
(import freja-layout/sizing/relative :as rs)
(import freja/collision :as c)
(use freja-jaylib)
(use freja/defonce)

(defonce canvas-state @{:width 300
                        :height 300
                        :zoom 1
                        :size 1
                        :hue 0.5
                        :actions @[]
                        :nof-undo 0})

(update canvas-state :color |(or $ [0 0 0]))
(update canvas-state :bg-color |(or $ :white))

(unless (canvas-state :render-texture)
  (put canvas-state :render-texture
       (load-render-texture
         (canvas-state :width)
         (canvas-state :height)))

  (begin-texture-mode (get canvas-state :render-texture))
  (clear-background 0x00000000)
  (end-texture-mode))

(put canvas-state :update
     (fn [self & args]
       (e/update! state/editor-state :canvas update ;args)
       self))

(put canvas-state :put
     (fn [self & args]
       (e/update! state/editor-state :canvas put ;args)
       self))

(e/put! state/editor-state :canvas canvas-state)

(defn draw
  [last-pos size pos color]
  (when last-pos
    (draw-line-ex [(inc (last-pos 0))
                   (last-pos 1)]

                  [(inc (pos 0))
                   (pos 1)]
                  size
                  color))

  (draw-pixel ;pos color))

(defn clear
  [state]
  (print "clearing")
  #  (clear-background 0x00000000)

  (draw-rectangle-rec [0 0 (state :width)
                       (state :height)]
                      (state :bg-color)))

(defn clear-undos
  [state]
  (def {:nof-undo nof-undo} state)
  (when (pos? nof-undo)
    (update state :actions
            (fn [actions]
              (array/slice actions 0 (- (length actions) nof-undo))))
    (put state :nof-undo 0)))

(defn begin-action
  [state]
  (clear-undos state)

  (put state :current-action @[]))

(defn push-action
  [state f]
  (clear-undos state)

  (update state
          (if (state :current-action)
            :current-action
            :actions)
          array/push
          f)
  (f))

(defn end-action
  [state]
  (:update state :actions array/push (state :current-action))
  (put state :current-action nil))

(defn zoom-pos
  [zoom [x y]]
  [(* zoom
      (math/floor (/ x zoom)))
   (* zoom
      (math/floor (/ y zoom)))])

(defn zoom-pos-smaller
  [zoom [x y]]
  [(* 1
      (math/floor (/ x zoom)))
   (* 1
      (math/floor (/ y zoom)))])

(defn undo
  [state]
  (def {:actions actions
        :current-action current-action} state)

  #(when current-action
  #  (end-action state))

  (:update state :nof-undo |(min (length actions) (inc $)))

  (begin-texture-mode (get state :render-texture))
  (clear state)

  (loop [i :range [0 (- (length actions) (state :nof-undo))]
         :let [f (get actions i)]]
    (if (array? f)
      (each ff f
        (ff))
      (f)))
  (end-texture-mode))

(defn redo
  [state]
  (def {:actions actions
        :nof-undo nof-undo} state)

  (when (pos? nof-undo)
    (begin-texture-mode (get state :render-texture))
    (:update state :nof-undo dec)
    (let [f (get actions (- (length actions) nof-undo))]
      (if (array? f)
        (each ff f
          (ff))
        (f)))
    (end-texture-mode)))

(defn v2*
  [[x y] v]
  [(math/floor (* x v)) (math/floor (* y v))])

(defn inner-draw
  [state pos]
  (def {:render-texture rt} state)

  (begin-texture-mode rt)

  (let [modpos (zoom-pos-smaller (state :zoom) pos)]
    (def args [(state :last-pos)
               (state :size)
               modpos
               (state :color)])
    (push-action state |(draw ;args))
    (put state :last-pos modpos))

  (end-texture-mode))

(defn inner-erase
  [state pos]
  (def {:render-texture rt} state)

  (let [modpos ;(zoom-pos-smaller (state :zoom) pos)
        [x y] modpos
        height (state :height)]
    (push-action state |(update-texture-rec
                          (get-render-texture rt)
                          [x (- height y) 4 4]
                          (array/new-filled (* 4 4) 0xff00ff00)))
    (put state :last-pos modpos)))


(defn handle-ev
  [state ev]
  (def {:render-texture rt
        :erase erase} state)

  (if erase
    (match ev
      [:press pos]
      (do
        (begin-action state)
        (inner-erase state pos))

      [:drag pos]
      (inner-erase state pos)

      [:release pos]
      (do
        (push-action state |(put state :last-pos nil))
        (end-action state)))

    (match ev
      [:scroll n]
      (if (pos? n)
        (:update state :zoom inc)
        (:update state :zoom dec))

      [:press pos]
      (do
        (begin-action state)
        (inner-draw state pos))

      [:drag pos]
      (inner-draw state pos)

      [:release pos]
      (do
        (push-action state |(put state :last-pos nil))
        (end-action state)))))


(defn hsv->rgb
  [h s v]
  (def i (math/floor (* h 6)))
  (def f (- (* h 6) i))
  (def p (* v (- 1 s)))
  (def q (* v (- 1 (* f s))))
  (def t (* v (- 1 (* (- 1 f) s))))
  (def c (case (mod i 6)
           0 @[v t p]
           1 @[q v p]
           2 @[p v t]
           3 @[p q v]
           4 @[t p v]
           5 @[v p q])))

(defonce color-picker-state @{})

(defn color-picker
  [props]
  (def {:width w
        :height h
        :canvas canvas} props)

  (def state color-picker-state)

  (def colors @{})

  (def ratio 0.5)

  (defn xy->color
    [{:width w :height h} x y]
    (if-let [c (get (get colors x) y)]
      (do c)
      (let [s (/ x w)
            v (- 1 (/ y h))
            c (hsv->rgb (canvas :hue) s v)]
        (put-in colors [x y] c)
        c)))

  (defn cp-handle-ev
    [el ev]
    (match ev
      ['(or (= (ev 0) :press)
            (= (ev 0) :drag)) [x y]]
      (-> canvas
          (:put :color (xy->color el x y))
          (:put :saturation (/ x (el :width)))
          (:put :value (- 1 (/ y (el :height)))))))

  (unless (state :rt)
    (put state :rt (load-render-texture w h)))

  (unless (= (canvas :hue) (state :last-hue))
    (begin-texture-mode (state :rt))
    (clear-background :white)
    (loop [x :range-to [0 (* ratio w)]
           y :range-to [0 (* ratio h)]
           :let [color (xy->color props (/ x ratio) (/ y ratio))]]
      (draw-pixel x
                  y
                  color))
    (end-texture-mode))

  (put state :last-hue (canvas :hue))

  (merge-into
    (dyn :element)
    @{:on-event
      (fn [self ev]
        (def ox (dyn :offset-x))
        (def oy (dyn :offset-y))

        (match ev
          [kind [x y]]
          (let [pos [(- x ox)
                     (- y oy)]]
            (when (c/in-rec? pos [0 0 (self :width) (self :height)])
              (cp-handle-ev self [kind pos])
              true))

          false))

      :state color-picker-state

      :render
      (fn [{:width w :height h} ox oy]
        (-> (get-render-texture (state :rt))
            (draw-texture-pro
              [0 (* h (- 1 ratio)) (* ratio w) (* ratio (- h))]
              [0 0 w h]
              [0 0]
              0
              :white)))

      :relative-sizing rs/block-sizing
      :children []
      :min-width w
      :min-height h
      :preset-width w
      :preset-height h
      :props {:width w
              :height h}}))


(def hue-picker-state @{})

(defn hue-picker
  [props]
  (def {:width w
        :height h
        :canvas canvas} props)

  (def state hue-picker-state)

  (def colors @{})

  (defn cp-handle-ev
    [el ev]
    (match ev
      ['(or (= (ev 0) :press)
            (= (ev 0) :drag)) [x y]]
      (-> canvas
          (:put :hue (/ x w))
          (:put :color (hsv->rgb (/ x w) (canvas :saturation) (canvas :value))))))

  (merge-into
    (dyn :element)
    @{:init
      (fn [self _]
        # hue picker
        (unless (state :rt)
          (put state :rt (load-render-texture w h))

          (begin-texture-mode (state :rt))
          (clear-background :white)
          (loop [x :range-to [0 w]
                 :let [color (hsv->rgb (/ x w) 1 1)]]
            (draw-line x 0
                       x h
                       color))
          (end-texture-mode)))

      :on-event
      (fn [self ev]
        (def ox (dyn :offset-x))
        (def oy (dyn :offset-y))

        (match ev
          [kind [x y]]
          (let [pos [(- x ox)
                     (- y oy)]]
            (when (c/in-rec? pos [0 0 (self :width) (self :height)])
              (cp-handle-ev self [kind pos])
              true))

          false))

      :state hue-picker-state

      :render
      (fn [{:width w :height h} ox oy]
        (-> (get-render-texture (state :rt))
            (draw-texture-pro
              [0 0 w (- h)]
              [0 0 w h]
              [0 0]
              0
              :white)))

      :relative-sizing rs/block-sizing
      :children []
      :min-width w
      :min-height h
      :preset-width w
      :preset-height h
      :props {:width w
              :height h}}))


(defn tools
  [props]

  (defn size-button
    [size]
    (def selected (= size (props :size)))
    [:clickable
     {:on-click (fn [_]
                  (:put props :size size))}
     [:background {:color (when selected
                            0x000000aa)}
      [:padding {:all 6}
       [:background {:color (if selected :white :black)}
        [:block {:height size}]]]]])

  (defn button
    [{:on-click f} & [label]]
    [:padding {:top 4}
     [:block {}
      [:clickable {:on-click f}
       [:padding {:all 6}
        label]]]])

  (defn zoom-button
    [level]
    (def selected (= (props :zoom) level))
    [:background {:color (when selected 0x000000aa)}
     [button {:on-click (fn [_] (:put props :zoom level))}
      [:text {:color (when selected :white)
              :size 22
              :text (string level)}]]])

  [:background {:color 0xaaaaaaff}
   [:padding {:all 6}
    [:block {}
     (size-button 1)]
    [:block {}
     (size-button 2)]
    [:block {}
     (size-button 3)]
    [:block {}
     (size-button 4)]
    [:block {}
     (size-button 5)]

    [:padding {:top 4}
     [:background {:color (when (props :erase)
                            0x000000aa)}
      [:block {}
       [:clickable {:on-click (fn [_]
                                (:update props :erase not))}
        [:padding {:all 6}
         [:text {:size 22
                 :color (when (props :erase)
                          :white)
                 :text
                 "Erase"}]]]]]]
    [button {:on-click (fn [_]
                         (print "huh?")

                         (begin-texture-mode (get props :render-texture))
                         (push-action
                           props
                           |(clear props))
                         (end-texture-mode))}
     [:text {:size 22
             :text "Clear"}]]
    [button {:on-click (fn [_]
                         (print "undo!")
                         (undo props))}
     [:text {:size 22
             :text "Undo"}]]
    [button {:on-click (fn [_] (redo props))}
     [:text {:size 22
             :text "Redo"}]]

    [:padding {:top 8}
     [:text {:size 18
             :text (string "Zoom: " (props :zoom))}]
     [:row {}
      (zoom-button 1)
      (zoom-button 4)
      (zoom-button 8)
      (zoom-button 12)]]]])

(defn canvas
  [props]
  (def {:width w
        :height h
        :render-texture rt} props)
  (merge-into (dyn :element)
              @{:on-event
                (fn [self ev]
                  (def ox (dyn :offset-x))
                  (def oy (dyn :offset-y))

                  (match ev
                    [kind [x y]]
                    (let [pos [(- x ox)
                               (- y oy)]]
                      (put props :last-mouse-pos pos)
                      (when (c/in-rec? (v2* pos (/ 1 (props :zoom)))
                                       [0 0 w h])
                        (handle-ev
                          props
                          [kind pos])
                        true))

                    [kind n [x y]]
                    (let [pos [(- x ox)
                               (- y oy)]]
                      (when (c/in-rec? (v2* pos (/ 1 (props :zoom)))
                                       [0 0 w h])
                        (handle-ev
                          props
                          [kind n pos])
                        true))

                    false))
                :render
                (fn [{:width w :height h} ox oy]
                  (-> (get-render-texture rt)
                      (draw-texture-pro
                        [0
                         0
                         (/ w (props :zoom))
                         (/ (- h) (props :zoom))]
                        [0 0 (* w 1) (* h 1)]
                        [0 0]
                        0
                        :white))

                  (when-let [[x y] #(props :last-mouse-pos)
                             (get-mouse-position)
                             x (- x ox)
                             y (- y oy)]
                    (if (and (> x 0)
                             (< x w))
                      (hide-cursor)
                      (show-cursor))
                    (draw-rectangle
                      (* (props :zoom)
                         (math/floor (/ x (props :zoom))))
                      (* (props :zoom)
                         (math/floor (/ y (props :zoom))))
                      (props :zoom)
                      (props :zoom)
                      :purple)))

                :relative-sizing rs/block-sizing
                :children []
                :preset-width (* (props :zoom) w)
                :preset-height (* (props :zoom) h)
                :props {:width (* (props :zoom) w)
                        :height (* (props :zoom) h)}}))

(defn hiccup
  [props & _]
  [:background {:color 0x000000ff}
   [:padding {:all 24
              :bottom 260}
    [:block {}
     [:row {}
      [:block {:min-width 75}
       [tools (props :canvas)]]
      [:block {}
       [:background {:color (get-in props [:canvas :bg-color])}
        [canvas (props :canvas)]]
       [:text {:color :white
               :size 22
               :text (string "Nof undos: " (get-in props [:canvas :nof-undo]))}]
       [:padding {:left 12}
        [:text {:color :white
                :size 22
                :text (string "Nof actions: " (length (get-in props [:canvas :actions])))}]]]

      [:text {:color :white
              :size 22
              :text (if (get-in props [:canvas :erase])
                      "Erasing"
                      "")}]
      [:block {:width 150 :height 150}
       [:background {:color :gray}
        [:padding {:all 6}
         [color-picker {:height 150
                        :width 150
                        :canvas (props :canvas)}]
         [:padding {:top 6}
          [hue-picker {:height 30
                       :width 150
                       :canvas (props :canvas)}]]
         [:padding {:top 6}
          [:background {:color (get-in props [:canvas :color])
                        :height 60
                        :width 60}]]]]]]]]])

(e/put! state/editor-state :right hiccup)

(comment
  (h/new-layer :pixel-editor
               hiccup
               props)

  (h/remove-layer :pixel-editor props)
  #
)
