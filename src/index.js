import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var storedState = localStorage.getItem('user');
var startingState = storedState ? JSON.parse(storedState) : null;

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: startingState
});

app.ports.storeUser.subscribe(function (user) {
	var postsJson = JSON.stringify(user);
    localStorage.setItem('user', postsJson);
});

window.addEventListener("storage", function(event) {
        if (event.storageArea === localStorage && event.key === storageKey) {
          app.ports.onStoreChange.send(event.newValue);
        }
      }, false);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
