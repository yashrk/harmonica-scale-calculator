# Harmonica scale chart generator

This program generates scale charts of different scales (natural, blues, pentatonic etc.) for given harp.

## Working sample

[Try it at yashrk.github.io](https://yashrk.github.io/harmonica-scales.html)

## How to run it locally

Install [Elm](http://elm-lang.org/). Run the following:
```
git clone git@github.com:yashrk/harmonica-scale-calculator.git
cd harmonica-scale-calculator
elm-reactor
```
and open `http://localhost:8000/App.elm` in your favourite browser.

## How to deploy it to your site

Run the following:
```
elm make App.elm --output harmonica-scales.html
```
and copy `harmonica-scales.html` to your site. No backend logic needed. Please observe AGPL if you are using or improve this code. Let me know if you like my application.

## How to embed it to your web page

Run the following:
```
elm-make App.elm --output harpcalc.js
```
and copy `harpcalc.js` to your site. Then edit your web page `<head>` and `<body>` sections, adding the following:
```
 <head>
   …
   <meta charset="UTF-8">
   <script src="harpcalc.js" type="text/javascript"></script>
 </head>
…
 <body>
   <script type="text/javascript">Elm.App.fullscreen()</script>
   …
 </body>
```
Please note you are accepting [GNU AGPL v.3](https://www.gnu.org/licenses/agpl-3.0.html) copyleft license agreement by using this code. You cannot use this code in AGPL-incompatible projects.

## License

This project is licensed under the GNU Affero General Public License v3.0 - see the [LICENSE](LICENSE) file for details.
