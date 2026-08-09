;; Copyright 2020-2026 The Defold Foundation
;; Copyright 2014-2020 King
;; Copyright 2009-2014 Ragnar Svensson, Christian Murray
;; Licensed under the Defold License version 1.0 (the "License"); you may not use
;; this file except in compliance with the License.
;;
;; You may obtain a copy of the License, together with FAQs at
;; https://www.defold.com/license
;;
;; Unless required by applicable law or agreed to in writing, software distributed
;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;; specific language governing permissions and limitations under the License.

(ns editor.debug-view-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [editor.debug-view :as debug-view]
            [editor.keymap :as keymap]
            [editor.localization :as localization]
            [editor.ui :as ui]
            [integration.test-util :as test-util])
  (:import [javafx.scene.control Button Control]
           [javafx.scene.layout VBox]))

(set! *warn-on-reflection* true)

(defn- make-editor-localization []
  (localization/make
    (test-util/make-test-prefs)
    :test
    {"en.editor_localization" #(io/reader (io/resource "localization/en.editor_localization"))}
    #(throw %)))

(def ^:private tool-bar-button-ids
  ["pause-debugger-button"
   "play-debugger-button"
   "step-in-debugger-button"
   "step-out-debugger-button"
   "step-over-debugger-button"
   "stop-debugger-button"])

(defn- tooltip-text [^VBox tool-bar ^String button-id]
  (let [^Control button (.lookup tool-bar (str "#" button-id))]
    (.getText (.getTooltip button))))

(deftest update-tool-bar-tooltips-test
  (let [localization (make-editor-localization)
        keymap (keymap/from keymap/empty :win32 {:debugger.continue {:add #{"F5"}}})
        tool-bar (ui/run-now
                   (let [tool-bar (VBox.)]
                     (doseq [^String id tool-bar-button-ids]
                       (.add (.getChildren tool-bar) (doto (Button.) (.setId id))))
                     (debug-view/update-tool-bar-tooltips!
                       {:console-grid-pane tool-bar
                        :keymap keymap
                        :localization localization})
                     tool-bar))]
    (testing "tooltip shows the shortcut when the command has a keybinding"
      (is (= "Continue (F5)" (tooltip-text tool-bar "play-debugger-button"))))
    (testing "tooltip is just the label when the command has no keybinding"
      (is (= "Break" (tooltip-text tool-bar "pause-debugger-button"))))))
