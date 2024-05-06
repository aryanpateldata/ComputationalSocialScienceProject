This Project was created by Aryan Patel and Gurnit Chauhan for Professor Davidson's class : Computational Sociology. Where we learned how to apply data science techniques to sociology problems. 


For this project we have chosen to create a Tool that will return words used in similar contexts as the input word. This may not always mean similar words sometimes antonyms can appear too but these
will show the the words that appear in most similar contexts to the input word you enter. This is created by using the ff-nn Word2Vec and specifically the CBOW feature of Word2Vector
(more information found here) https://en.wikipedia.org/wiki/Word2vec.  

**information about what the returned words and similarity mean: Continuous Bag of Words (CBOW) is a type of Word2Vec model that aims to learn word embeddings by predicting a target word based on its surrounding context words. In CBOW, the model is trained to predict the target word from a context window of adjacent words. The word embeddings learned by CBOW can capture semantic similarities between words based on their distributional properties in the training corpus.
In practice, the word embeddings produced by CBOW are often effective at capturing words with similar meanings or contexts. This is because words that frequently co-occur in similar contexts tend to have similar embeddings. As a result, when you compute the similarity between word embeddings using cosine similarity, words that are semantically or contextually similar often have higher similarity scores. However, it's essential to note that CBOW and Word2Vec, in general, are unsupervised models that learn word embeddings based solely on the distributional patterns of words in the training data. While they can capture many semantic similarities, they may not always perfectly capture all nuances of word meaning or context. Additionally, the quality of word embeddings learned by CBOW can be influenced by factors such as the size and quality of the training corpus, the dimensionality of the embeddings, and the specific hyperparameters used during training. In summary, while CBOW in Word2Vec generally returns similar words in meaning or context, the extent of similarity captured by the word embeddings depends on various factors**










The model was trained on around 1000 songs lyrics collected from scrapping Shazam Lyrics. Then we processed the data, and trained the model on the song lyrics and created an embedding matrix. Using this we
created a similartiy matrix and then a function that would return the most similar words (in context not meaning) to the input word. We have used this approach for faster loading of the application
as training word2vec on the data each time would be computationally expensive.  The App will return a list of most similar words (contextually) shown by histogram and chart in first tab. in the second tab there is a word netword graph with slider that can also explain similar information. 


Using the returned words from the application , someone can use this application to write and compose song lyrics as it was specifcally trained on song lyrics. Someone can enter for example if they wanted to use a word like "cash"
and they entered it in the input box this would return : 
bentley	0.6251799
ten	0.5803557
pour	0.5788494
wave	0.5787163
hanging	0.5718196
surf	0.5575727
box	0.5400224
sa	0.5264146
racks	0.5241919
hop	0.5210371 . 

Using this similar words someone could see they could replace their original word with racks or another similar word in composition of their music to find more relevant terms then what they are inputting. 

Difficulties : It was difficult to use GeniusAPi in R so we were forced to manually scrape lyrics which was more time intensive then sending API Calls. This model was trained on around
1000 songs but the more data we could have collected the more relevant and contextually similar words it would be able to return. 


