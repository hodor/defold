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

(ns editor.lsp.project
  (:require [clojure.string :as string]
            [dynamo.graph :as g]
            [editor.core :as core]
            [editor.defold-project :as defold-project]
            [editor.lsp.async :as lsp.async]
            [editor.lsp.server :as lsp.server]
            [editor.resource :as resource]
            [editor.util :as util]
            [editor.workspace :as workspace]
            [util.coll :as coll]
            [util.defonce :as defonce]
            [util.eduction :as e])
  (:import [java.net URI]))

(set! *warn-on-reflection* true)

(def ^:private ^:const completion-item-kind-reference 18)

(defn- uri->resource [project uri evaluation-context]
  (let [basis (:basis evaluation-context)
        workspace (g/node-value project :workspace evaluation-context)]
    (when-let [proj-path (workspace/as-proj-path basis workspace (.getPath (URI. uri)))]
      (workspace/find-resource basis workspace proj-path))))

(defn- gui-scene-node-ids-using-script [basis gui-script-node-id]
  (coll/into-> (g/targets-of basis gui-script-node-id :resource) #{}
    (keep (fn [[node-id input-label]]
            (when (and (= :script-resource input-label)
                       (g/node-kw-instance? basis :editor.gui/GuiSceneNode node-id))
              node-id)))))

(defn- component-source-node-ids-requiring-module [basis requiring-resource-infos-by-module-proj-path module-proj-path]
  (coll/into-> (g/pre-traverse
                 basis
                 [[module-proj-path nil]]
                 (fn [_basis [proj-path _node-id]]
                   (requiring-resource-infos-by-module-proj-path proj-path))) #{}
    (mapcat (fn [[proj-path node-id]]
              (case (resource/filename->type-ext proj-path)
                "script" [node-id]
                "gui_script" (gui-scene-node-ids-using-script basis node-id)
                nil)))))

;; A component source (script, gui scene) reaches its owning ReferencedComponent
;; through the source-resource arc: directly for non-overridable component types,
;; through an override node for overridable ones.
(defn- owning-game-object-node-ids [basis component-source-node-ids]
  (coll/into-> component-source-node-ids #{}
    (mapcat #(e/cons % (g/overrides basis %)))
    (mapcat #(g/targets-of basis % :resource))
    (keep (fn [[node-id input-label]]
            (when (and (= :source-resource input-label)
                       (g/node-kw-instance? basis :editor.game-object/ReferencedComponent node-id))
              (some->> node-id
                       (core/owner-node-id basis)
                       (g/override-root basis)))))))

(defn- complete-component-ids [owner-root-ids line character content evaluation-context]
  (->> owner-root-ids
       (e/mapcat #(coll/keys (g/node-value % :component-ids evaluation-context)))
       (e/distinct)
       (coll/sort util/natural-order)
       (mapv (fn [component-id]
               {:label component-id
                :kind completion-item-kind-reference
                :textEdit {:range {:start {:line line
                                           :character (- character (dec (count content)))}
                                   :end {:line line
                                         :character character}}
                           :newText component-id}}))))

(defn- complete-urls [project owner-root-ids socket line character url-content evaluation-context]
  (if (coll/empty? owner-root-ids)
    []
    (let [basis (:basis evaluation-context)
          replacement-start (- character (count url-content))]
      (->> (g/node-value project :nodes-by-resource-path evaluation-context)
           (e/mapcat (fn [[_ node-id]]
                       (when (g/node-kw-instance? basis :editor.collection/CollectionNode node-id)
                         (let [go-inst-ids (g/node-value node-id :go-inst-ids evaluation-context)
                               go-instance-ids (coll/vals go-inst-ids)]
                           (when (coll/any?
                                   (fn [go-instance-id]
                                     (owner-root-ids
                                       (g/override-root
                                         basis
                                         (g/node-value go-instance-id :source-id evaluation-context))))
                                   go-instance-ids)
                             go-instance-ids)))))
           (e/mapcat (fn [go-instance-id]
                       (let [source-id (g/node-value go-instance-id :source-id evaluation-context)
                             url (g/node-value go-instance-id :url evaluation-context)
                             save-value (g/node-value source-id :save-value evaluation-context)]
                         (e/cons
                           url
                           (e/map #(str url "#" (:id %))
                                  (e/concat (:components save-value)
                                            (:embedded-components save-value)))))))
           (e/map #(cond->> % socket (str socket ":")))
           (e/distinct)
           (coll/sort util/natural-order)
           (mapv (fn [url]
                   {:label url
                    :kind completion-item-kind-reference
                    :textEdit {:range {:start {:line line
                                               :character replacement-start}
                                       :end {:line line
                                             :character character}}
                               :newText url}}))))))

;; The animation-name argument position of an animation-playing call whose first
;; argument references a component in the same game object.
(defn- animation-argument-component-id [text-before-string]
  (second (re-find #"\b(?:sprite\s*\.\s*play_flipbook|model\s*\.\s*play_anim)\s*\(\s*[\"']#([^\"'/:]*)[\"']\s*,\s*$"
                   text-before-string)))

(defn- source-animation-ids [basis source-node-id evaluation-context]
  (let [node-type (g/node-type* basis source-node-id)
        label (coll/first-where #(g/has-output? node-type %) [:anim-ids :animation-ids])
        anim-ids (when label (g/node-value source-node-id label evaluation-context))]
    (when-not (g/error-value? anim-ids)
      anim-ids)))

(defn- complete-animation-ids [owner-root-ids component-id line character content evaluation-context]
  (let [basis (:basis evaluation-context)
        replacement-start (- character (count content))]
    (->> owner-root-ids
         (e/keep #(get (g/node-value % :component-ids evaluation-context) component-id))
         (e/keep #(ffirst (g/sources-of basis % :source-resource)))
         (e/mapcat #(source-animation-ids basis % evaluation-context))
         (e/distinct)
         (coll/sort util/natural-order)
         (mapv (fn [anim-id]
                 {:label anim-id
                  :kind completion-item-kind-reference
                  :textEdit {:range {:start {:line line
                                             :character replacement-start}
                                     :end {:line line
                                           :character character}}
                             :newText anim-id}})))))

(defn- completion-owner-root-ids [project resource-node-id resource evaluation-context]
  (let [basis (:basis evaluation-context)]
    (owning-game-object-node-ids
      basis
      (case (resource/type-ext resource)
        "script" #{resource-node-id}
        "gui_script" (gui-scene-node-ids-using-script basis resource-node-id)
        (component-source-node-ids-requiring-module
          basis
          (g/node-value
            (defold-project/script-intelligence project evaluation-context)
            :requiring-resource-infos-by-module-proj-path
            evaluation-context)
          (resource/proj-path resource))))))

(defn- reference-completion-string [resource-node-id line character evaluation-context]
  (when-let [line-text (get (g/node-value resource-node-id :lines evaluation-context) line)]
    (when (<= 0 character (count line-text))
      (let [^String line-prefix (subs line-text 0 character)]
        (loop [index 0
               quote \u0000
               string-start -1]
          (if (= index (.length line-prefix))
            (when (and (not= \u0000 quote)
                       (not (re-find #"\brequire\s*\(?\s*$" (subs line-prefix 0 string-start))))
              (coll/pair (subs line-prefix (inc string-start))
                         (subs line-prefix 0 string-start)))
            (let [character (.charAt line-prefix index)]
              (if (= \u0000 quote)
                (cond
                  (and (= \- character)
                       (< (inc index) (.length line-prefix))
                       (= \- (.charAt line-prefix (inc index))))
                  nil

                  (or (= \" character)
                      (= \' character))
                  (recur (inc index) character index)

                  :else
                  (recur (inc index) quote string-start))
                (cond
                  (= \\ character)
                  (recur (long (min (+ index 2) (.length line-prefix))) quote string-start)

                  (= quote character)
                  (recur (inc index) \u0000 -1)

                  :else
                  (recur (inc index) quote string-start))))))))))

(defn- completion [project {{:keys [uri]} :textDocument
                            {:keys [line character]} :position}]
  (let [items
        (lsp.async/with-auto-evaluation-context evaluation-context
          (or
            (when-let [requested-resource (uri->resource project uri evaluation-context)]
              (when-let [resource-node-id (defold-project/get-resource-node project requested-resource evaluation-context)]
                (when-let [[content text-before-string] (reference-completion-string resource-node-id line character evaluation-context)]
                  (let [animation-component-id (animation-argument-component-id text-before-string)]
                    (cond
                      animation-component-id
                      (complete-animation-ids
                        (completion-owner-root-ids project resource-node-id requested-resource evaluation-context)
                        animation-component-id
                        line
                        character
                        content
                        evaluation-context)

                      (string/starts-with? content "#")
                      (complete-component-ids
                        (completion-owner-root-ids project resource-node-id requested-resource evaluation-context)
                        line
                        character
                        content
                        evaluation-context)

                      (string/starts-with? content "/")
                      (complete-urls
                        project
                        (completion-owner-root-ids project resource-node-id requested-resource evaluation-context)
                        nil
                        line
                        character
                        content
                        evaluation-context)

                      :else
                      (when-let [[_ socket] (re-matches #"([^#:]+):(?:/.*)?" content)]
                        (complete-urls
                          project
                          (completion-owner-root-ids project resource-node-id requested-resource evaluation-context)
                          socket
                          line
                          character
                          content
                          evaluation-context)))))))
            []))]
    {:isIncomplete true
     :items items}))

(defonce/record ProjectLauncher [project]
  lsp.server/Launcher
  (launch [_ _directory]
    (lsp.server/launch-connection
      {"initialize"
       (fn [_]
         {:capabilities
          {:textDocumentSync {:openClose false
                              :change 0}
           :completionProvider {:resolveProvider false
                                :triggerCharacters ["#" "/" ":"]}}})

       "initialized" (constantly nil)
       "shutdown" (constantly nil)
       "exit" (constantly nil)
       "textDocument/completion" (partial completion project)})))

(defn language-server [project]
  {:languages #{"lua"}
   :launcher (->ProjectLauncher project)})
