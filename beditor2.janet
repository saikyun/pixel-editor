(import freja/events :as e)
(import freja/state)
(import freja/hiccup :as h)
(import freja-layout/sizing/relative :as rs)
(import freja/collision :as c)
(use freja-jaylib)
(use freja/defonce)

(defonce canvas-state @{:width 400
                        :height 400})

(put canvas-state :actions @[])
(put canvas-state :nof-undo 0)

(merge-into canvas-state @{:size 1})

(update canvas-state :color |(or $ :green))
(update canvas-state :bg-color |(or $ :white))

(unless (canvas-state :render-texture)
  (put canvas-state :render-texture
       (load-render-texture
         (canvas-state :width)
         (canvas-state :height))))


(begin-texture-mode (get canvas-state :render-texture))
(clear-background 0x00000000)
(end-texture-mode)

(put canvas-state :update
     (fn [_ & args]
       (e/update! state/editor-state :canvas update ;args)))

(put canvas-state :put
     (fn [_ & args]
       (e/update! state/editor-state :canvas put ;args)))

(e/put! state/editor-state :canvas canvas-state)

(defn draw
  [state pos]
  (def {:last-pos lp
        :size size} state)
  (if lp
    (draw-line-ex lp
                  pos
                  size
                  (if (state :erase)
                    (state :bg-color)
                    (state :color)))
    (draw-pixel ;pos :green))

  (put state :last-pos pos))

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

(defn undo
  [state]
  (def {:actions actions} state)

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

(defn handle-ev
  [state ev]
  (def {:render-texture rt} state)

  (begin-texture-mode rt)

  (match ev
    [:press pos]
    (do (begin-action state)
      (push-action state |(draw state pos)))

    [:drag pos]
    (push-action state |(draw state pos))

    [:release pos]
    (do (push-action state |(put state :last-pos nil))
      (end-action state)))

  (end-texture-mode))

(defn tools
  [props]

  (defn size-button
    [size]
    (def selected (= size (props :size)))
    [:clickable
     {:on-click (fn [_]
                  (:put props :size size)
                  (:put props :color :green))}
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
             :text "Redo"}]]]])

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
                      (when (c/in-rec? pos [0 0 w h])
                        (handle-ev
                          props
                          [kind pos])
                        true))

                    false))
                :render
                (fn [{:width w :height h} x y]
                  (-> (get-render-texture rt)
                      (draw-texture-pro
                        [0 0 (* 1 w) (* 1 (- h))]
                        [0 0 w h]
                        [0 0]
                        0
                        :white)))
                :relative-sizing rs/block-sizing
                :children []
                :preset-width w
                :preset-height h
                :props {:width w :height h}}))

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
                :text (string "Nof actions: " (length (get-in props [:canvas :actions])))}]]]]]]])

(e/put! state/editor-state :right hiccup)

(comment
  (h/new-layer :pixel-editor
               hiccup
               props)

  (h/remove-layer :pixel-editor props)
  #
)
