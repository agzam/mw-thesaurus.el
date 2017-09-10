(require 'mw-thesaurus)

(defvar mw-thesaurus--test-data-xml
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

(defvar mw-thesaurus--test-data-expected-parsed
  "* decline [noun]
** a gradual sinking and wasting away of mind or body
   - her sad /decline/ from a robust athlete to an old woman with arthritis
*** Synonyms:
    debilitation, decay, decaying, declension, degeneration, descent, deterioration, ebbing, enfeeblement, weakening
*** Related words:
    atrophy
    exhaustion
    drooping, flagging, limping
    regression, relapse, setback
*** Near antonyms:
    invigoration, strengthening
    progress
    rejuvenation, rejuvenescence
*** Antonyms:
    comeback, improvement, rally, recovery, recuperation, rehabilitation, revitalization, snapback
** a change to a lower state or level
   - the /decline/ of the Roman Empire
*** Synonyms:
    decadence, declension, declination, degeneracy, degeneration, degradation, dégringolade, descent, deterioration, devolution, downfall, downgrade, ebb, eclipse, fall
*** Related words:
    dark age, nadir, sunset
    decay, rotting, spoiling
    breakup, crumbling, decomposition, disintegration, dissolution
    abasement, debasement
    depreciation, lessening
    decimation, demolishment, demolition, desolation, destruction, havoc, ruin, ruination
    abatement, decrease, decrement, de-escalation, deflation, diminishment, diminution, dip, downslide, downtrend, downturn, drop, drop-off, falloff, loss, lowering, reduction, sag, shrinkage, slip, slump
*** Near antonyms:
    advancement, development, evolution, growth
    blossoming, flourishing, flowering
    renewal, restoration, revitalization
    heightening
    accretion, accrual, addendum, addition, augmentation, boost, enhancement, gain, increase, increment, raise, supplement
*** Antonyms:
    ascent, rise, upswing
** a downward slope
   - the bicyclist lost control on the unexpectedly steep /decline/
*** Synonyms:
    declension, declivity, descent, dip, downgrade, downhill, fall, hang, hanging
*** Related words:
    basin, depression, hollow
*** Near antonyms:
    glacis, grade, gradient, hill, inclination, incline, lean, pitch, rake, tilt
*** Antonyms:
    acclivity, ascent, rise, upgrade, uphill, uprise
** a loss of status
   - the engagement at the small club was an unmistakable sign of the rock band's /decline/
*** Synonyms:
    decline, dégringolade, demise, descent, down, downfall, fall, flameout, Götterdämmerung
*** Related words:
    breakdown, burnout, collapse, crash, meltdown, ruin, undoing
    defeat, disappointment, reversal, setback
    bottom, nadir
    abasement, disgrace, humiliation
*** Near antonyms:
    advance, headway, progress
    flower, heyday, prime
*** Antonyms:
    aggrandizement, ascent, exaltation, rise, up
** the amount by which something is lessened
   - a huge /decline/ in the value of the artwork after its authenticity was questioned
*** Synonyms:
    abatement, decline, decrement, dent, depletion, depression, diminishment, diminution, drop, drop-off, fall, falloff, loss, reduction, shrinkage, step-down
*** Related words:
    deduction, subtraction
    downturn, slip, slump
    curtailment, cut, cutback, retrenchment, shortening
*** Near antonyms:
    accretion, accrual, accumulation, addition, supplement
    continuation, extension
    upswing, uptrend, upturn
*** Antonyms:
    boost, enlargement, gain, increase, increment, raise, rise, step-up, uptick* decline [verb]
** to show unwillingness to accept, do, engage in, or agree to
   - he /declined/ the invitation to the party
*** Synonyms:
    balk (at), deselect, disapprove, negative, nix, pass, pass up, refuse, reject, reprobate, repudiate, spurn, throw out, throw over, turn down
*** Related words:
    blow off, disdain, rebuff, scorn, scout, shoot down
    overrule, veto
    forbid, prohibit, proscribe
    dismiss, ignore
    abstain (from), forbear, refrain (from)
    deny, disavow, disclaim, dispute, gainsay
    stick
    abjure, forswear (/also/ foreswear), recant, renounce, retract, take back, unsay, withdraw
    avoid, bypass, detour
    contradict, deny, disown, negate
    controvert, disagree (with), disprove, dispute, rebut, refute
    back down, back off, backtrack
    disallow, recall, renege, revoke
*** Near antonyms:
    condone, countenance, swallow, tolerate
    adopt, embrace, receive, take, welcome
    accede, acquiesce, agree, assent, consent
    choose, handpick, select
    espouse, support
*** Antonyms:
    accept, agree (to), approve
** to be unwilling to grant
   - /declined/ our request to hold a party
*** Synonyms:
    decline, disallow, disapprove, negative, nix, refuse, reject, reprobate, withhold
*** Related words:
    ban, enjoin, forbid, prohibit, proscribe, veto
    rebuff, repel, spurn
    check, constrain, curb, hold, keep, repress, restrain, restrict
    balk (at), hinder, impede, obstruct
*** Near antonyms:
    afford, furnish, give, provide, supply
    authorize, commission, license (/also/ licence)
    accede (to), acquiesce, agree (to), assent (to), consent (to), warrant
    accord, sanction, vouchsafe
*** Antonyms:
    allow, concede, grant, let, OK (/or/ okay), permit
** to go to a lower level especially abruptly
   - new-car sales /declined/ to their lowest level in years
*** Synonyms:
    crash, crater, decline, descend, dip, dive, fall, lower, nose-dive, plummet, plunge, sink, skid, tumble
*** Related words:
    abate, decrease, de-escalate, die (down), diminish, droop, dwindle, ebb, lessen, let up, moderate, subside, taper off, wane
    recede, retreat
*** Near antonyms:
    accumulate, balloon, build, burgeon (/also/ bourgeon), enlarge, escalate, expand, grow, increase, intensify, mushroom, pick up, snowball, swell, wax
*** Antonyms:
    arise, ascend, lift, mount, rise, soar, spike, up
** to become worse or of less value
   - his reputation as a writer began to /decline/ not long after his death
*** Synonyms:
    atrophy, crumble, decay, decline, degenerate, descend, devolve, ebb, regress, retrograde, rot, sink, worsen
*** Related words:
    abate, de-escalate, diminish, downsize, dwindle, recede, wane
    break down, corrupt, decompose, degrade, dilapidate, disintegrate, molder, putrefy
    sour, spoil
    lessen, lower, reduce
    debilitate, undermine
    droop, fail, fall, flag, lag, languish, run down, sag, slip, waste (away), weaken, wilt
*** Near antonyms:
    better, upgrade
    enhance, enrich, fortify, heighten, intensify, strengthen
    advance, develop, march, proceed, progress
*** Antonyms:
    ameliorate, improve, meliorate
** to grow less in scope or intensity especially gradually
   - the winds should /decline/ as soon as the cold front passes
*** Synonyms:
    abate, decline, de-escalate, die (away /or/ down /or/ out), diminish, drain (away), drop (off), dwindle, ease, ebb, fall, fall away, lessen, let up, lower, moderate, pall, phase down, ratchet (down) /also/ rachet (down), recede, relent, remit, shrink, subside, taper, taper off, wane
*** Related words:
    compress, condense, constrict, contract
    evaporate, fade (away), fritter (away), give out, melt (away), peter (out), tail (off), vanish
    slacken, slow (down)
    alleviate, relax
    flag, sink, weaken
    cave (in), collapse, deflate
*** Near antonyms:
    appear, emerge, show up
    blow up, distend, elongate, lengthen
*** Antonyms:
    accumulate, balloon, build, burgeon (/also/ bourgeon), enlarge, escalate, expand, grow, increase, intensify, mount, mushroom, pick up, rise, snowball, soar, swell, wax
** to lead or extend downward
   - the bike path /declines/ toward the riverbank and then follows the river for several miles
*** Synonyms:
    decline, dip, drop, fall, plunge, sink
*** Related words:
    angle, cant, cock, heel, incline, lean, list, recline, slant, slope, tilt, tip
*** Near antonyms:
    even, flatten, level, plane, smooth, straighten
*** Antonyms:
    arise, ascend, climb, mount, rise, uprise, upsweep, upturn")

(defvar mw-thesaurus--word-not-exist-xml
  "
<?xml version=\"1.0\" encoding=\"utf-8\"?>
<entry_list version=\"1.0\">
  <suggestion>jab</suggestion>
  <suggestion>table</suggestion>
  <suggestion>ad-lib</suggestion>
</entry_list>")

(ert-deftest mw-thesaurus--parse-test ()
  (let* ((xml (with-temp-buffer
               (insert mw-thesaurus--test-data-xml)
               (xml-parse-region (point-min) (point-max))))
         (parsed-org-text (mw-thesaurus--parse xml)))
    (should-not (equal "--" parsed-org-text))
    (should (equal mw-thesaurus--test-data-expected-parsed parsed-org-text))))

(ert-deftest mw-thesaurus--parse-not-existing-word ()
  (let* ((xml (with-temp-buffer
                (insert mw-thesaurus--word-not-exist-xml)
                (xml-parse-region (point-min) (point-max))))
         (parsed-org-text (mw-thesaurus--parse xml)))
    (should (equal "" parsed-org-text))))
