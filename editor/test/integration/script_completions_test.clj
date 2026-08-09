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

(ns integration.script-completions-test
  (:require [clojure.test :refer :all]
            [dynamo.graph :as g]
            [integration.test-util :as test-util]))

(set! *warn-on-reflection* true)

(deftest component-id-completions-test
  (test-util/with-loaded-project
    (testing "script attached to a game object completes its component ids"
      (let [script-node (test-util/resource-node project "/logic/main.script")
            completions (get (g/node-value script-node :completions) "#")]
        (is (= ["camera" "gui" "script" "session_proxy"] (mapv :name completions)))
        (is (every? #(= "/logic/main.go" (:detail %)) completions))))
    (testing "gui script completes the component ids of the hosting game object"
      (let [gui-script-node (test-util/resource-node project "/logic/main.gui_script")
            completions (get (g/node-value gui-script-node :completions) "#")]
        (is (= ["camera" "gui" "script" "session_proxy"] (mapv :name completions)))
        (is (every? #(= "/logic/main.go" (:detail %)) completions))))
    (testing "animation ids of a sprite component are completed by its play call"
      (let [ball-script (test-util/resource-node project "/logic/session/ball.script")
            completions (g/node-value ball-script :completions)]
        (is (= ["test"] (mapv :name (get completions "#anim:sprite"))))))
    (testing "component ids follow graph changes"
      (let [script-node (test-util/resource-node project "/logic/main.script")
            go-node (test-util/resource-node project "/logic/main.go")
            camera-node (get (g/node-value go-node :component-ids) "camera")]
        (g/transact (g/delete-node camera-node))
        (is (= ["gui" "script" "session_proxy"]
               (mapv :name (get (g/node-value script-node :completions) "#"))))))))

(deftest url-completions-test
  (test-util/with-loaded-project
    (testing "script completes the urls of game objects in its collection"
      (let [session-script (test-util/resource-node project "/logic/session/session.script")
            urls (into #{} (map :name) (get (g/node-value session-script :completions) "url"))]
        ;; session.collection hosts the "session" and "hud" game objects
        (is (contains? urls "/session"))
        (is (contains? urls "/hud"))
        ;; and each game object's components are addressable
        (is (contains? urls "/session#script"))
        (is (contains? urls "/session#level01_proxy"))
        (is (contains? urls "/hud#gui"))))
    (testing "urls cover referenced, embedded and nested game objects"
      (let [props-script (test-util/resource-node project "/script/props.script")
            urls (into #{} (map :name) (get (g/node-value props-script :completions) "url"))]
        ;; props.script is a component of the referenced "props" game object and
        ;; the embedded "props_embedded" game object in props.collection, which
        ;; is in turn nested in sub_props.collection under the "props" instance
        (is (contains? urls "/props"))
        (is (contains? urls "/props_embedded"))
        (is (contains? urls "/props/props"))))
    (testing "a parented game object sees its collection siblings, and embedded components get full urls"
      (let [ball-script (test-util/resource-node project "/logic/session/ball.script")
            urls (into #{} (map :name) (get (g/node-value ball-script :completions) "url"))]
        ;; ball is parented under paddle in base_level.collection but still sees every sibling
        (is (contains? urls "/paddle"))
        (is (contains? urls "/left_wall"))
        (is (contains? urls "/roof"))
        ;; an embedded component resolves to /instance#id, never a bare #id
        (is (contains? urls "/ball#co"))
        (is (not (contains? urls "#co")))))
    (testing "urls follow graph changes"
      (let [session-script (test-util/resource-node project "/logic/session/session.script")
            session-collection (test-util/resource-node project "/logic/session/session.collection")
            hud-instance (get (g/node-value session-collection :go-inst-ids) "hud")]
        (g/transact (g/set-property hud-instance :id "heads_up"))
        (let [urls (into #{} (map :name) (get (g/node-value session-script :completions) "url"))]
          (is (contains? urls "/heads_up"))
          (is (not (contains? urls "/hud"))))))
    ;; destructive: clearing an instance path must run last in this shared fixture
    (testing "an instance with no prototype does not break url completion"
      (let [ball-script (test-util/resource-node project "/logic/session/ball.script")
            base-level (test-util/resource-node project "/logic/session/base_level.collection")
            left-wall (get (g/node-value base-level :go-inst-ids) "left_wall")]
        (g/transact (g/set-property left-wall :path {:resource nil :overrides []}))
        (is (some? (get (g/node-value ball-script :completions) "url")))))))
