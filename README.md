# AI Paradigms and AI Safety:
### Mapping Artefacts and Techniques to Safety Issues


**Publication**: Jose Hernández-Orallo, Fernando Martínez-Plumed, Shahar Avin, Jess Whittlestone, Seán Ó hÉigeartaigh, [*"AI Paradigms and AI Safety:Mapping Artefacts and Techniques to Safety Issues"*](https://doi.org/10.3233/FAIA200386), Proceedings of the [24th European Conference on Artificial Intelligence (ECAI 2020)](http://ecai2020.eu/),  Santiago de Compostela, Spain, September 2020.
---

In this work, we present a structured approach for thinking about paradigms in AI and use this as the basis for empirical analysis of how AI safety issues have been explored in the research literature so far. 

We define **paradigms** in AI more precisely, drawing on literature in the philosophy of science to distinguish two different types of paradigms in AI: **artefacts** and **techniques**. Drawing on existing research and the expertise of several AI and AI safety researchers, we outline a preliminary taxonomy of ten different AI artefacts. 

To conduct a more grounded empirical analysis of the historical evolution of AI techniques, artefacts and safety issues, we work with [AI topics](https://aitopics.org/search), an official database from the AAAI, using the documents from the period 1970-2017. This archive contains a variety of documents related to AI research (news, blog entries, conferences, journals and other repositories) that are collected automatically with NewsFinder \cite{buchanan2013virtual}. We divide the archive into {\em research} documents and non-research documents. From the ~111K documents gathered,  ~11K are research papers and the remaining ~100K are mostly media. With a mapping approach between the list of exemplars (tokens) of [tecniques](https://github.com/nandomp/AIParadigmsSafety/blob/master/AI%20topics%20tags/mapping-techniques.csv) and [artefacts](https://github.com/nandomp/AIParadigmsSafety/blob/master/AI%20topics%20tags/mapping-artefacts.csv), and the tags obtained from \textit{AI topics} (substrings appearing in titles, abstracts and metadata), we summarise the trends in a series of plots where the data has been smoothed with a moving average filter in order to reduce short-term volatility in data, and each area stack is scaled to sum to 100\% in every certain period of time:


### Evolution of Techniques

<div align="center">
<img src="https://github.com/nandomp/AIParadigmsSafety/blob/master/Plots/mapping-artefacts-noSafety_all.png"
      alt="" width="800" />
</div>

### Evolution of Artefacts

<div align="center">
<img src="https://github.com/nandomp/AIParadigmsSafety/blob/master/Plots/mapping-techniques-noSafety_all.png"
      alt="" width="800" />
</div>

### Evolution of Techniques (only AI safety-related documents)

<div align="center">
<img src="https://github.com/nandomp/AIParadigmsSafety/blob/master/Plots/mapping-artefacts-Safety_all.png"
      alt="" width="800" />
</div>

### Evolution of Artefacts (only AI safety-related documents)

<div align="center">
<img src="https://github.com/nandomp/AIParadigmsSafety/blob/master/Plots/mapping-artefacts-Safety_all.png"
      alt="" width="800" />
</div>

Next we look more closely at how different AI paradigms relate to specific safety issues. To do this, we categorise different safety issues, identifying key terms in surveys, blogs and events in AI safety and clustered them into [groups](https://github.com/nandomp/AIParadigmsSafety/blob/master/AI%20topics%20tags/mapping-safetyissues.csv). With this list of categories, we then analyse how related different paradigms and safety issues are, by counting the number of papers (of those filtered for safety relevance) which mention both a given paradigm (technique or artefact) and a given safety issue, for all combinations of techniques and safety issues (and the same analysis, separately, for artefacts):



### Mapping between Techniques and Safety issues

<div align="center">
<img src="https://github.com/nandomp/AIParadigmsSafety/blob/master/Plots/Techniques_vs_SafeIssues_v5.png"
      alt="" width="800" />
</div>

### Mapping between Artefacts and Safety issues

<div align="center">
<img src="https://github.com/nandomp/AIParadigmsSafety/blob/master/Plots/Artefacts_vs_SafeIssues_v5.png"
      alt="" width="800" />
</div>


Our analysis identifies a number of gaps in AI safety research where certain combinations of techniques, paradigms and safety issues need to be addressed.

