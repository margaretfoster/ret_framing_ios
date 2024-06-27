## Welcome!

This repository contains code and data to accompany "Rhetorical Framing in Inter-Governmental Negotiations" by [Margaret J. Foster](https://github.com/margaretfoster) and [Tana Johnson](https://lafollette.wisc.edu/people/johnson-tana/).

We focus on how power plays out in international organizations, here the [Committee on Trade and Development](https://www.wto.org/english/tratop_e/devel_e/d3ctte_e.htm) in the World Trade Organization. 
In particular, we look at how states use rhetorical framing (using particular words or terms to characterize an issue and imply appropriate actions in line with that characterization). 

Our manuscript argues that rhetorical framing is used by both conventionally weak and strong states due to its dual offensive/defensive utility and relatively low resource requirements.

We model how this plays out in over 5,000 speaker paragraphs from meetings 1-113, showing that rhetorical framing is prevalent, employed even by “very weak” states.

This repository contains the code to produce visualization in the manuscript-- many of which are included here in color (in addition to the printer-friendly grayscale of the manuscript).

The [data](https://github.com/margaretfoster/ret_framing_ios/tree/master/data) that we used was extracted from 113-Word and pdf [transcripts produced by the WTO](https://docs.wto.org/dol2fe/Pages/FE_Search/FE_S_S005.aspx).
We extracted the data, extracted speaker information using SpaceyR, and extensively validated paragraph breaks, speaker identification, and content. We also standardized many of the references. The result is the WTOSpeakerTurnsM1to113.xlsx.

In the manuscript, we qualitatively describe the interplay between two pervasive frames: 
- A "Reciprocation Frame," associated with conventionally more powerful states focuses on the responsibilities of all states and pushes for two-way concessions.
- A "Redistribution Frame," associated with conventionally weak states that emphasizes injustices and prescribe one-way concessions to aid those states.

We note that proponents of both frames make great efforts to avoid having the other frame dominate the committee minutes. 

We support this analysis with visualizations showing the sequencing and quantity of turns associated with conventionally strong and weaker states (proxied by World Bank income categories). Below, we briefly introduce some of the visualizations. (The manuscript replications are greyscale; color is here for effect.)

We use state income level to proxy "Strength" and Frame, as the narrow issue area and specific rhetorical context of diplomatic meetings renders this corpus especially difficult to identify and extract frames at scale. If you are interested in attempts to use Machine Learning to tag rhetorical frames, you might be interested in the [companion repository](https://github.com/margaretfoster/wto_classification_sim) that benchmarks various classification algorithms. 
Our analysis visualizes turn sequence and meeting dominance. 

For example, this plot of speaker-turns per meeting shows delegates from Lower Middle Income Countries (LMC) --- often champions of the Redistribution Frame-- matching the within-meeting activity levels of delegates from High Income Countries (HIC), who are most associated with the Reciprocation Frame. 

![Speaker-Frequency Interplay](https://github.com/margaretfoster/ret_framing_ios/blob/master/images/incomebyyear.png)

We also find evidence that although they contribute less often to the debate, delegations from Low-Income Countries (LICs)-- aka those delegations who would be expected to have the fewest resources -- tend to position their contributions towards the opening and closing of any given meeting. We interpret this as strategically interjecting at the outset and close to influencing the committee's trajectory while minimizing the load on their delegations. 


![Turn distributions](https://github.com/margaretfoster/ret_framing_ios/blob/master/images/speakerTurnsPosition.png)

Finally, we randomly selected three meetings to plot the sequence of strong and weak states. We show that both conventionally "weak" and conventionally "strong" states display a pattern of responding to the other holding the floor. We interpret this as a defensive strategy to prevent the opposing frame from becoming the defacto dominant frame.

![Turn sequence](https://github.com/margaretfoster/ret_framing_ios/blob/master/images/threeSequenceColor.png)


