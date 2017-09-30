<div id="main"></div>

<script type="text/javascript">
{% include assets/pounce.js %}
</script>
<script>
var node = document.getElementById('main');
var app = Elm.Main.embed(node);
</script>

## Playing the Game

The game board has a grid of forty-nine squares which are all open at the start of the game.

**Setup:** The first player selects a square and places his piece there with the mouse. The second player then places his piece on a different square. (Pouncing is not allowed on the first turn).

**How to move:** After the players have placed their pieces, they take turns moving them around the board by selecting the square they want to move to with the mouse. Pieces move like queens in chess (that is to say, any number of squares vertically, horizontally or diagonally), with the following caveat. Each square moved _from_ is filled in and becomes a block, and no player may occupy that square again, or move over or through it.

**How to win:** You can win in two ways. When your opponent is on a square you may legally move to, you can capture him by moving to that square yourself. This is called _pouncing_. You also win when your opponent has no legal moves left, i.e., all his adjacent squares are filled. This is called _trapping_. Of course, you lose if you are trapped, or if your opponent pounces on you.

## Postmortem

I created this project to give myself a concrete problem to solve in Elm as a motivation to learn the language. Here are some thoughts about how it went:

**Does Elm live up to its reputation as a better approach to front end development?** Yes! I have been avoiding front-end web development for years because of the poor integration of HTML and JavaScript, and because my brain just refuses to grok JavaScript. Elm provides a unified platform for front end development, allowing a uniform syntax for representing all objects in the browser. Because it is a language rather than a markup, every part of the page is dynamic by default. The Elm architecture is a simple way to organize an application with a structured MVC pattern, which make starting development easy.

**What would I do differently?** The biggest mistake in implementation was splitting the project into game controller and Board module early on. Coming from object-oriented programming, this seemed like a natural thing to do (and it gave me practice with Elm's module system). But the split introduced a number of complications, particularly with the models and message routing. The main model ended up being mostly the Board model: because the board needs to react to all states of the game, there is little benefit to separating it from the model for the game. The main Msg needs to be aware of Board Msg and destructure and create that type. The code would be less convoluted if it all resided in a single file. The single-file pattern is the standard recommendation by the Elm community -- don't create modules until absolutely needed -- but I chose to ignore that in the beginning. My second project will follow the single-file pattern more closely until size forces a real need for a split.

**Concurrency:** The biggest challenge in creating Pounce was inserting a call to view between the call to update from the user and the call to update for the computer player. The end of the user update is creation of a Task to run the robot code. But the task ran immediately rather than producing an intervening call to view. After discussion on the Elm Slack, I put a 50ms Process.sleep before the task that produced the command to run the robot -- this sleep forced Elm's concurrency code to prefer running the view. This pattern is troubling, as it depends on a side effect of the delay -- what if in the future a 100ms delay is required? Why should I need a delay at all? The Elm documentation indicates the concurrency library is still being worked on, and I trust Evan to produce an elegant solution to this problem eventually. But it highlights Elm is still a work in progress.
