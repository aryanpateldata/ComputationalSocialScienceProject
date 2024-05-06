This Project was created by Aryan Patel and Gurnit Chauhan for Professor Davidson's class : Computational Sociology. Where we learned how to apply data science techniques to sociology problems. 


For this project we have chosen to create a Tool that will return words used in similar contexts as the input word. This may not always mean similar words sometimes antonyms can appear too but these
will show the the words that appear in most similar contexts to the input word you enter. This is created by using the ff-nn Word2Vec and specifically the CBOW feature of Word2Vector
(more information found here) https://en.wikipedia.org/wiki/Word2vec. 

The model was trained on around 1000 songs lyrics collected from scrapping Shazam Lyrics. Then we processed the data, and trained the model on the song lyrics and created an embedding matrix. Using this we
created a similartiy matrix and then a function that would return the most similar words (in context not meaning) to the input word. We have used this approach for faster loading of the application
as training word2vec on the data each time would be computationally expensive. 


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


