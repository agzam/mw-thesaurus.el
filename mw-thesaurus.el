;;; mw-thesaurus.el --- Merriam-Webster Thesaurus -*- lexical-binding: t; -*-
;;
;; Filename: mw-thesaurus.el
;; Description: Thesaurus look up through www.dictionaryapi.com.
;; Author: Ag Ibragimov
;; Maintainer: Ag Ibragimov (concat "agzam.ibragimov" "@" "gm" "ail" ".c" "om")
;; Copyright (C) 2017  Ag Ibragimov

;; Keywords: synonyms thesaurus dictionary
;; Version: 0.0.1

;;; Commentary:

;; Thesaurus look up through www.dictionaryapi.com - Merriam-Webster online dictionary
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For more than 150 years, in print and now online, Merriam-Webster has been America's leading and most-trusted provider of language information.
;; Each month, Merriam-Webster web sites offer guidance to more than 40 million visitors. In print, publications include Merriam-Webster's Collegiate Dictionary (among the best-selling books in American history) and newly published dictionaries for English-language learners.
;; All Merriam-Webster products and services are backed by the largest team of professional dictionary editors and writers in America, and one of the largest in the world.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'request nil t)

(defcustom mw-thesaurus--api-key
  "67d977d5-790b-412e-a547-9dbcc2bcd525"
  "Merriam-Webster API access key")

(defcustom mw-thesaurus--base-url
  "http://www.dictionaryapi.com/api/v1/references/thesaurus/xml/"
  "Merriam-Webster API base URL")

(defvar test-data
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<entry_list version=\"1.0\">
  <entry id=\"decline\">
    <term>
      <hw>decline</hw>
    </term>
    <fl>noun</fl>
    <sens>
      <sn>1</sn>
      <mc>a gradual sinking and wasting away of mind or body</mc>
      <vi>her sad <it>decline</it> from a robust athlete to an old woman with arthritis</vi>
      <syn>debilitation, decay, decaying, declension, degeneration, descent, deterioration, ebbing, enfeeblement, weakening</syn>
      <rel>atrophy; exhaustion; drooping, flagging, limping; regression, relapse, setback</rel>
      <near>invigoration, strengthening; progress; rejuvenation, rejuvenescence</near>
      <ant>comeback, improvement, rally, recovery, recuperation, rehabilitation, revitalization, snapback</ant>
    </sens>
    <sens>
      <sn>2</sn>
      <mc>a change to a lower state or level</mc>
      <vi>the <it>decline</it> of the Roman Empire</vi>
      <syn>decadence, declension, declination, degeneracy, degeneration, degradation, dégringolade, descent, deterioration, devolution, downfall, downgrade, ebb, eclipse, fall</syn>
      <rel>dark age, nadir, sunset; decay, rotting, spoiling; breakup, crumbling, decomposition, disintegration, dissolution; abasement, debasement; depreciation, lessening; decimation, demolishment, demolition, desolation, destruction, havoc, ruin, ruination; abatement, decrease, decrement, de-escalation, deflation, diminishment, diminution, dip, downslide, downtrend, downturn, drop, drop-off, falloff, loss, lowering, reduction, sag, shrinkage, slip, slump</rel>
      <near>advancement, development, evolution, growth; blossoming, flourishing, flowering; renewal, restoration, revitalization; heightening; accretion, accrual, addendum, addition, augmentation, boost, enhancement, gain, increase, increment, raise, supplement</near>
      <ant>ascent, rise, upswing</ant>
    </sens>
    <sens>
      <sn>3</sn>
      <mc>a downward slope</mc>
      <vi>the bicyclist lost control on the unexpectedly steep <it>decline</it></vi>
      <syn>declension, declivity, descent, dip, downgrade, downhill, fall, hang, hanging</syn>
      <rel>basin, depression, hollow</rel>
      <near>glacis, grade, gradient, hill, inclination, incline, lean, pitch, rake, tilt</near>
      <ant>acclivity, ascent, rise, upgrade, uphill, uprise</ant>
    </sens>
    <sens>
      <sn>4</sn>
      <mc>a loss of status</mc>
      <vi>the engagement at the small club was an unmistakable sign of the rock band's <it>decline</it></vi>
      <syn>decline, dégringolade, demise, descent, down, downfall, fall, flameout, Götterdämmerung</syn>
      <rel>breakdown, burnout, collapse, crash, meltdown, ruin, undoing; defeat, disappointment, reversal, setback; bottom, nadir; abasement, disgrace, humiliation</rel>
      <near>advance, headway, progress; flower, heyday, prime</near>
      <ant>aggrandizement, ascent, exaltation, rise, up</ant>
    </sens>
    <sens>
      <sn>5</sn>
      <mc>the amount by which something is lessened</mc>
      <vi>a huge <it>decline</it> in the value of the artwork after its authenticity was questioned</vi>
      <syn>abatement, decline, decrement, dent, depletion, depression, diminishment, diminution, drop, drop-off, fall, falloff, loss, reduction, shrinkage, step-down</syn>
      <rel>deduction, subtraction; downturn, slip, slump; curtailment, cut, cutback, retrenchment, shortening</rel>
      <near>accretion, accrual, accumulation, addition, supplement; continuation, extension; upswing, uptrend, upturn</near>
      <ant>boost, enlargement, gain, increase, increment, raise, rise, step-up, uptick</ant>
    </sens>
  </entry>
  <entry id=\"decline\">
    <term>
      <hw>decline</hw>
    </term>
    <fl>verb</fl>
    <sens>
      <sn>1</sn>
      <mc>to show unwillingness to accept, do, engage in, or agree to</mc>
      <vi>he <it>declined</it> the invitation to the party</vi>
      <vi>she <it>declined</it> to participate in the soccer game</vi>
      <syn>balk (at), deselect, disapprove, negative, nix, pass, pass up, refuse, reject, reprobate, repudiate, spurn, throw out, throw over, turn down</syn>
      <rel>blow off, disdain, rebuff, scorn, scout, shoot down; overrule, veto; forbid, prohibit, proscribe; dismiss, ignore; abstain (from), forbear, refrain (from); deny, disavow, disclaim, dispute, gainsay; stick; abjure, forswear (<it>also</it> foreswear), recant, renounce, retract, take back, unsay, withdraw; avoid, bypass, detour; contradict, deny, disown, negate; controvert, disagree (with), disprove, dispute, rebut, refute; back down, back off, backtrack; disallow, recall, renege, revoke</rel>
      <ph>turn one's back on</ph>
      <near>condone, countenance, swallow, tolerate; adopt, embrace, receive, take, welcome; accede, acquiesce, agree, assent, consent; choose, handpick, select; espouse, support</near>
      <ant>accept, agree (to), approve</ant>
    </sens>
    <sens>
      <sn>2</sn>
      <mc>to be unwilling to grant</mc>
      <vi><it>declined</it> our request to hold a party</vi>
      <syn>decline, disallow, disapprove, negative, nix, refuse, reject, reprobate, withhold</syn>
      <rel>ban, enjoin, forbid, prohibit, proscribe, veto; rebuff, repel, spurn; check, constrain, curb, hold, keep, repress, restrain, restrict; balk (at), hinder, impede, obstruct</rel>
      <near>afford, furnish, give, provide, supply; authorize, commission, license (<it>also</it> licence); accede (to), acquiesce, agree (to), assent (to), consent (to), warrant; accord, sanction, vouchsafe</near>
      <ant>allow, concede, grant, let, OK (<it>or</it> okay), permit</ant>
    </sens>
    <sens>
      <sn>3</sn>
      <mc>to go to a lower level especially abruptly</mc>
      <vi>new-car sales <it>declined</it> to their lowest level in years</vi>
      <syn>crash, crater, decline, descend, dip, dive, fall, lower, nose-dive, plummet, plunge, sink, skid, tumble</syn>
      <rel>abate, decrease, de-escalate, die (down), diminish, droop, dwindle, ebb, lessen, let up, moderate, subside, taper off, wane; recede, retreat</rel>
      <near>accumulate, balloon, build, burgeon (<it>also</it> bourgeon), enlarge, escalate, expand, grow, increase, intensify, mushroom, pick up, snowball, swell, wax</near>
      <ant>arise, ascend, lift, mount, rise, soar, spike, up</ant>
    </sens>
    <sens>
      <sn>4</sn>
      <mc>to become worse or of less value</mc>
      <vi>his reputation as a writer began to <it>decline</it> not long after his death</vi>
      <syn>atrophy, crumble, decay, decline, degenerate, descend, devolve, ebb, regress, retrograde, rot, sink, worsen</syn>
      <rel>abate, de-escalate, diminish, downsize, dwindle, recede, wane; break down, corrupt, decompose, degrade, dilapidate, disintegrate, molder, putrefy; sour, spoil; lessen, lower, reduce; debilitate, undermine; droop, fail, fall, flag, lag, languish, run down, sag, slip, waste (away), weaken, wilt</rel>
      <near>better, upgrade; enhance, enrich, fortify, heighten, intensify, strengthen; advance, develop, march, proceed, progress</near>
      <ant>ameliorate, improve, meliorate</ant>
    </sens>
    <sens>
      <sn>5</sn>
      <mc>to grow less in scope or intensity especially gradually</mc>
      <vi>the winds should <it>decline</it> as soon as the cold front passes</vi>
      <syn>abate, decline, de-escalate, die (away <it>or</it> down <it>or</it> out), diminish, drain (away), drop (off), dwindle, ease, ebb, fall, fall away, lessen, let up, lower, moderate, pall, phase down, ratchet (down) <it>also</it> rachet (down), recede, relent, remit, shrink, subside, taper, taper off, wane</syn>
      <rel>compress, condense, constrict, contract; evaporate, fade (away), fritter (away), give out, melt (away), peter (out), tail (off), vanish; slacken, slow (down); alleviate, relax; flag, sink, weaken; cave (in), collapse, deflate</rel>
      <near>appear, emerge, show up; blow up, distend, elongate, lengthen</near>
      <ant>accumulate, balloon, build, burgeon (<it>also</it> bourgeon), enlarge, escalate, expand, grow, increase, intensify, mount, mushroom, pick up, rise, snowball, soar, swell, wax</ant>
    </sens>
    <sens>
      <sn>6</sn>
      <mc>to lead or extend downward</mc>
      <vi>the bike path <it>declines</it> toward the riverbank and then follows the river for several miles</vi>
      <syn>decline, dip, drop, fall, plunge, sink</syn>
      <rel>angle, cant, cock, heel, incline, lean, list, recline, slant, slope, tilt, tip</rel>
      <near>even, flatten, level, plane, smooth, straighten</near>
      <ant>arise, ascend, climb, mount, rise, uprise, upsweep, upturn</ant>
    </sens>
  </entry>
</entry_list>")

(defun mw-thesaurus/get-entires (tree)
  (let* ((entry-list (assq 'entry_list tree)))
      (xml-get-children entry-list 'entry)))

(defun get-xml-node (root path)
  (let* ((current-node (xml-get-children root (car path))))
    (if (< 1 (length path))
        (get-xml-node (car current-node) (cdr path))
      current-node)))

(defun mw-thesaurus/get-title (entry)
  (-> (get-xml-node entry '(term hw))
      car (seq-drop 2) car))

(defun mw-thesaurus/get-type (entry)
  (-> (get-xml-node entry '(fl))
      car (seq-drop 2) car))

(defun mw-thesaurus/italicize (prop)
  (let* ((its      (get-xml-node prop '(it))))
    (mapconcat (lambda (e)
                 (if (member e its)
                     (concat "/" (-> e last car string-trim) "/")
                   (when (stringp e ) e))) prop "")))

(defun mw-thesaurus/snd-subs (article)
  (let* ((whole-sub (-> article
                        (get-xml-node '(vi)) car))
         (sub-str (mw-thesaurus/italicize whole-sub)))
    (concat "   - " sub-str)))

(defun mw-thesaurus/other-tag (article tag-type)
  (let* ((content (-> article
                        (get-xml-node `(,tag-type))
                        car
                        mw-thesaurus/italicize))
         (title (cond
                 ((eq tag-type 'syn) "Synonyms")
                 ((eq tag-type 'rel) "Related words")
                 ((eq tag-type 'near) "Near antonyms")
                 ((eq tag-type 'ant) "Antonyms")
                 (t "Unknown type"))))
    (when (and content (< 0 (length content)))
      (string-join (list "\n*** " title ":\n    " (s-replace ";" "\n   " content)) ""))))

(defun mw-thesaurus/third-lvl (article)
  (let* ((syns (mw-thesaurus/other-tag article 'syn))
         (rels (mw-thesaurus/other-tag article 'rel))
         (nears (mw-thesaurus/other-tag article 'near))
         (ants (mw-thesaurus/other-tag article 'ant)))
    (string-join (list syns rels nears ants) "")))

(defun mw-thesaurus/snd-level (entry)
  (let ((articles (get-xml-node entry '(sens))))
    (mapconcat
     (lambda (article)
       (let* ((desc (-> (get-xml-node article '(mc))
                       car mw-thesaurus/italicize))
              (snd-subs (mw-thesaurus/snd-subs article))
              (third-lvl (mw-thesaurus/third-lvl article)))
         (string-join (list "** " desc "\n"
                            snd-subs third-lvl) "")))
     articles "\n")))

(defun mw-thesaurus/text (data)
  (mapconcat
   (lambda (entry)
     (let* ((fst-level (concat "* " (mw-thesaurus/get-title entry)
                               " [" (mw-thesaurus/get-type entry) "]\n"))
            (snd-level (mw-thesaurus/snd-level entry)))
       (string-join (list fst-level snd-level) "")))
   (mw-thesaurus/get-entires data) ""))

(defun mw-thesaurus/lookup (data)
  (let* ((temp-buf  (generate-new-buffer "* Thesaurus *")))
    (set-buffer temp-buf)
    (with-current-buffer temp-buf
      (funcall 'org-mode)
      (insert (mw-thesaurus/text data)))
    (switch-to-buffer-other-window temp-buf)))

(defun mw-thesaurus/lookup-at-point ()
  (interactive)
  (let ((word (word-at-point)))
    (request
     (concat (symbol-value 'mw-thesaurus--base-url)
             word "?key="
             (symbol-value 'mw-thesaurus--api-key))
     :parser (lambda () (xml-parse-region (point-min) (point-max)))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (mw-thesaurus/lookup data))))))

(provide 'mw-thesaurus)
