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

(ns editor.code.view-test
  (:require [clojure.test :refer :all]
            [editor.code.data :as data]
            [editor.code.view :as view]))

(set! *warn-on-reflection* true)

(def ^:private anim-grammar
  {:string-argument-completion-patterns
   [{:pattern #"sprite\.play_flipbook\s*\(\s*[\"']#([a-zA-Z0-9_-]+)[\"']\s*,\s*[\"']([a-zA-Z0-9_-]*)$"
     :context-format "#anim:%s"}]})

(defn- completion-context
  ([line trigger-characters]
   (completion-context line trigger-characters nil))
  ([line trigger-characters grammar]
   (let [cursor (data/->Cursor 0 (count line))]
     (view/produce-completion-context
       {:lines [line]
        :cursor-ranges [(data/Cursor->CursorRange cursor)]
        :completion-trigger-characters trigger-characters
        :grammar grammar}))))

(deftest produce-completion-context-test
  (testing "dotted prefix"
    (let [context (completion-context "socket.d" #{"." "#"})]
      (is (= "socket" (:context context)))
      (is (= "d" (:query context)))
      (is (= "d" (:trigger context)))))
  (testing "hash at the start of a string"
    (let [context (completion-context "msg.post(\"#" #{"." "#"})]
      (is (= "#" (:context context)))
      (is (= "" (:query context)))
      (is (= "#" (:trigger context)))))
  (testing "hash with a partial component id"
    (let [context (completion-context "msg.post(\"#play_s" #{"." "#"})]
      (is (= "#" (:context context)))
      (is (= "play_s" (:query context)))
      (is (= (data/->Cursor 0 11) (:insert-cursor context)))))
  (testing "hash in a single-quoted string"
    (let [context (completion-context "msg.post('#cam" #{"." "#"})]
      (is (= "#" (:context context)))
      (is (= "cam" (:query context)))))
  (testing "hash query keeps digits and hyphens"
    (let [context (completion-context "msg.post(\"#nil-2" #{"." "#"})]
      (is (= "#" (:context context)))
      (is (= "nil-2" (:query context)))))
  (testing "hash as the length operator is not a component id context"
    (let [context (completion-context "local n = #" #{"." "#"})]
      (is (= "" (:context context)))
      (is (= "" (:query context)))))
  (testing "hash context requires the hash trigger character"
    (let [context (completion-context "msg.post(\"#play_s" #{"."})]
      (is (= "" (:context context)))
      (is (= "play_s" (:query context)))))
  (testing "animation argument produces a component-scoped context"
    (let [context (completion-context "sprite.play_flipbook(\"#hero\", \"ru" #{"." "#"} anim-grammar)]
      (is (= "#anim:hero" (:context context)))
      (is (= "ru" (:query context)))))
  (testing "animation argument matches without a space after the comma"
    (let [context (completion-context "sprite.play_flipbook('#hero','" #{"." "#"} anim-grammar)]
      (is (= "#anim:hero" (:context context)))
      (is (= "" (:query context)))))
  (testing "a string starting with a slash is a url context"
    (let [context (completion-context "msg.post(\"/" #{"." "#" "/"})]
      (is (= "url" (:context context)))
      (is (= "/" (:query context)))))
  (testing "url query keeps the whole path including nesting and the component"
    (let [context (completion-context "msg.post(\"/enemies/boss#spr" #{"." "#" "/"})]
      (is (= "url" (:context context)))
      (is (= "/enemies/boss#spr" (:query context)))))
  (testing "a division is not a url context"
    (let [context (completion-context "local half = width /" #{"." "#" "/"})]
      (is (= "" (:context context)))
      (is (= "" (:query context)))))
  (testing "url context requires the slash trigger character"
    (let [context (completion-context "msg.post(\"/enemies" #{"." "#"})]
      (is (not= "url" (:context context))))))
