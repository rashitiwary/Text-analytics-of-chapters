**Sentiment analysis**

For this project, the dataset comprises an extensive document structured into 11 chapters, identified by Roman numerals (e.g., "I", "II"). To efficiently manage and process the text, a dictionary was created using the "which" function in R to organize the chapters. Once segregated, all the chapters were loaded into a VCorpus.

To enhance the text analytics process, I implemented several techniques, including the removal of stop words, the computation of Document-Term Matrix (DTM) and Term-Document Matrix (TDM), and the conversion of text into lowercase. Additionally, tokenization was applied to break down the text into individual units, and TF-IDF (Term Frequency-Inverse Document Frequency) was utilized to assign importance scores to words based on their frequency and rarity across the entire document.
These text analytics concepts collectively contribute to a more refined and insightful analysis of the document content.
