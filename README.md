# [Senate_Tracker](https://aholmes23.shinyapps.io/SenateTracker/)
<!DOCTYPE html>
<html>

<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">

  <link rel="stylesheet" href="https://stackedit.io/style.css" />
</head>

      
<ul>
<li><a href="#first-things-first">First Things First</a></li>
<li><a href="#maths-stuff-books">Maths Stuff 📚</a>
<ul>
<li><a href="#seasonality">Seasonality</a></li>
<li><a href="#autocorrelation">Autocorrelation</a></li>
<li><a href="#decomposition">Decomposition</a></li>
<li><a href="#additive">Additive</a></li>
<li><a href="#multiplicative">Multiplicative</a></li>
<li><a href="#stl-my-fav">STL (My Fav)</a></li>
<li><a href="#seats--x11-methods">Seats & X11 methods</a>
<ul>
<li><a href="#x11">X11</a></li>
<li><a href="#seats">SEATS</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>


  </div>
  <div class="stackedit__right">
    <div class="stackedit__html">
      <h1 id="bulb-user-guide">💡 User Guide</h1>
<h2 id="first-things-first">First Things First</h2>
<p>This guide is designed to bring <em><strong>YOU</strong></em> up to speed on what this app is all about.<br>
First, I would like to thank you for your interest in this project 😊</p>
<p>To get started exploring stock trades and when politicians bought/sold them, click on the side bar menu and find <strong>‘Home’</strong> (🏠)</p>
<p>From this page you can explore any of the <em><strong>8,000+</strong></em> available stocks to choose from but <em><strong>not</strong></em> all will have senate trades.<br>
The table to the right of the time series plot are monthly stock quotes for your selected stock. This updates daily <font color="red">‼️</font><br>
The table below the plot features ANY trades that senators have made and discolsed. (A neato pop-up will inform if no senators have traded the selected stonk)<br>
The sidebar also houses a variable selection. Pick from any to see it within the plot.</p>
<p>Red 🔴/Green🟢/Orange🟠/Blue🔵 lines appear on the plot indicating the <code>Type</code> of transaction the senator did and when they did.</p>
<p>Let’s keep it goin!<br>
So what about maths with stonks🚀 That’s why I’m here!<br>
Alright alright<br>
<sub>I was getting to it</sub></p>
<h2 id="maths-stuff-books">Maths Stuff 📚</h2>
<h3 id="seasonality">Seasonality</h3>
<p>Click on the ‘Seasonality’ (🍃) tab to see the seasonality of the selected stock. The plot next to it will be the yearly breakdown by color of the seasonality patterns📈.<br>
This makes finding seasonal trends easier.<br>
Another nifty feature is that <em><strong>YOU</strong></em> can decide how this decomposition is run. More on this lil later.</p>
<blockquote>
<p>‘Decomposition’ (in this case) is us trying to make sense of the past by taking into account certain features of the series (Trend, Seasonality, Random)</p>
</blockquote>
<hr>
<h3 id="autocorrelation">Autocorrelation</h3>
<p>As we continue down the sidebar, we arrive at this thing called ‘Autocorrelation’ (📊) tab<br>
…<br>
What is that you ask?<br>
Correlation with lags and <em><strong>YOU</strong></em> get to choose how much lag is present (plus animations)<br>
woo hoo!🎉</p>
<blockquote>
<p>Lag is just how far we are pushing earlier observations down and seeing how they correlate with those values.</p>
</blockquote>
<p><em>Table example:</em></p>

<table>
<thead>
<tr>
<th>Month</th>
<th>SomeNum</th>
<th>Lagged Value</th>
</tr>
</thead>
<tbody>
<tr>
<td>1</td>
<td>2</td>
<td>NA</td>
</tr>
<tr>
<td>2</td>
<td>3</td>
<td>2</td>
</tr>
</tbody>
</table><p>See how the first month got pushed down to the second month?<br>
That’s Autocorrelation! ‘Correlation with lags’</p>
<p>Next Menu Item!</p>
<hr>
<h3 id="decomposition">Decomposition</h3>
<p>Decomposition (🗑️) tab is all about breaking down a series into different parts.<br>
There are 5️⃣ options to choose when performing decomposition on a time series.<br>
Classical contains both additive and multiplicative types. Seasonality does not change which is usually not a good thing.</p>
<h3 id="additive">Additive</h3>
<p>Concerned about <em>adding</em> up the different components.</p>
<p>Trend<sup>i</sup> ➕ Seasonality<sup>i</sup> ➕ Random <strong>=</strong> Y<sup>i</sup></p>
<blockquote>
<ul>
<li>Use Additive when the trend is mostly going one direction and seasonal variance is minimal</li>
<li>Expressed in ‘things’ above the trend i.e. ‘We are $400 above the trend’</li>
</ul>
</blockquote>
<hr>
<h3 id="multiplicative">Multiplicative</h3>
<p>Concerned about <em>multiplying</em> the different components together.</p>
<p>Trend<sup>i</sup> ✖️ Seasonality<sup>i</sup> ✖️ Random<sup>i</sup> <strong>=</strong> Y<sup>i</sup></p>
<blockquote>
<ul>
<li>Use Multiplicative when the trend is parabolic or the seasonal variation is getting ‘cone’ shaped.</li>
<li>Expressed in percentages above the trend (middle is 0%)</li>
</ul>
</blockquote>
<hr>
<h3 id="stl-my-fav">STL (My Fav)</h3>
<p><em>Seasonal and Trend decomposition using Loess</em><br>
Uses additive but multiplicative can be obtained through transformations of data.</p>
<blockquote>
<ul>
<li>Uses locally fitted regression models to fit the line and bring out the true features of the series as well as being robust to outliers.</li>
<li>Read more about it here! <a href="https://www.scb.se/contentassets/ca21efb41fee47d293bbee5bf7be7fb3/stl-a-seasonal-trend-decomposition-procedure-based-on-loess.pdf"><code>STL.pdf</code></a></li>
</ul>
</blockquote>
<hr>
<h3 id="seats--x11-methods">Seats &amp; X11 methods</h3>
<h4 id="x11">X11</h4>
<p>Developed by the US Census Bureau for better forecasting and evaluation of census data. It only allows for monthly and quarterly data. 😔</p>
<p>Classical decomp but on steroids! Seasonality allowed to change.<br>
Many extra steps involved we won’t dive into here but know its awesome.</p>
<h4 id="seats">SEATS</h4>
<p>(<em>Seasonal Extraction in ARIMA Time Series</em>) was developed by the Bank of Spain. Seasonality is also allowed to change here as well.<br>
It’s a pretty great method.</p>
<blockquote>
<p>Learn just how these methods do it <a href="https://www2.census.gov/software/x-13arima-seats/x-13-data/documentation/docx13as.pdf"><code>X-13ARIMA-SEATS Manual</code></a></p>
</blockquote>
<hr>
<p>Play around and test all different stuff!<br>
This is <em><strong>YOUR</strong></em> playground for discovering stock patterns 🚀 or politician ‘shadiness’ <font color="red">!! </font></p>


    
 
</body>

</html>
