Text Prediction App
========================================================
author: Iñigo López Pérez
date: 2016 April

Capstone project - Data Science Specialization [JHU]


Goal and How to
========================================================


<div style = "font-size:60%")>
The goal of the project is design of a <strong>text prediction algorithm</strong> and an implementation of the algorithm as a <strong>Shiny web app</strong>.

How works:
<br>
<ol>
<li> You can enter a sentence in the <strong>text input field</strong> on the left and the <strong>predicted word</strong> appears below.</li>
<li> The main panel, on right, contains a tab with a table of <strong>extended prediction</strong> with the 5 most probable words, and their probabilities.</li>
<li> The main panel has a secondary tab containing the extended <strong>app documentation</strong> and some references.</li>
</ol>

App: 
<br>
You can also find the app at  <a href="https://inyigolopez.shinyapps.io/wordsPredictionApp/">https://inyigolopez.shinyapps.io/wordsPredictionApp/</a>.
</div>  

Try the app!
========================================================

<h3><font color="FireBrick">An embedded live version.</font></h3>

<iframe src="https://inyigolopez.shinyapps.io/wordsPredictionApp/" height=600 width=1000></iframe>

How does it work?
========================================================

<div style = "font-size:70%")>

<p> The algorithm was trained using a set of English sentences, sampled from three different web sources: blogs, news and twitter. Cross validation was used to asess the accuracy of the predictions.</p>

<p>The model takes the last few words of a sentence (a n-gram when n words are used) and uses statistics about a large collection of English sentences to find the most probable next word, given that few words.</p> 

<p>Technically, the probabilities displayed by the app are assigned using a <a href="http://en.wikipedia.org/wiki/N-gram" target="_blank">4-gram model</a>  and implementation of the <a href="http://en.wikipedia.org/wiki/Katz%27s_back-off_model" target="_blank">Katz Backoff algorithm</a>.</p>

<p>
The resulting model is stored in a set of data files: a file for unigrams, a file for bigrams, a file for trigram and another for fourgram. The information in these files is similar to the <a href="http://www.speech.sri.com/projects/srilm/manpages/ngram-format.5.html">ARPA format files</a> for n-gram models. The complete 4-gram model size is 80Mb in disk space.</p>

</div>

References
========================================================

<div style = "font-size:70%")>
<p>

<ol>
<li><a href="http://en.wikipedia.org/wiki/N-gram" target="_blank">N-gram construction</a></li>
<li>Katz, S. (1987). *Estimation of probabilities from sparse data for the language model component of a speech recognizer*. Acoustics, Speech and Signal Processing, IEEE Transactions on, 35(3), 400-401. </li>
<li><a href="http://en.wikipedia.org/wiki/Katz%27s_back-off_model" target="_blank">Katz Backoff algorithm</a></li>
<li><a href="https://www.coursera.org/course/nlp" target="_blank">Stanford Coursera Course for Natural Language Processing</a></li>
<li>Deeper documentation in <a href="https://github.com/Inyigolopez/datascienceCapstoneProject" target="_blank">Github inyigolopez</a></li>
</ol>

</p>
Thanks you!
</div>
