# Yoga Tree SVG

The SVG Renderer for [Yoga Tree](https://pursuit.purescript.org/packages/purescript-yoga-tree/1.0.0/docs/Yoga.Tree#t:Tree), implemented using [Halogen](https://github.com/purescript-halogen/purescript-halogen).

* Allows user to specify their own component for rendering items in preview or directly in the tree;
* Supports keyboard navigation;
* Allows users to enter a section of the tree;
* Supports zoom;
* Allows pinning/unpinning items to a separate list;
* Shows node preview on hover;
* See `./screens` folder for [Screenshots](https://github.com/shamansir/purescript-yoga-tree-svg/tree/main/screens);
* Allows user to turn on/off parts of the UI:
    * History;
    * Pinned items;
    * Location / Selection;
    * Preview;
    * Editing tree as Text;
    * Export to JSON/Text;
    * Zoom controls;
    * Fold view;


## TODO

* Better layout;
* Take path of a section from URL `hash` as well;
* Support optional depth render limit;
* Support optional children count limitl;
* Multiple selection;
* Better fold view:
    * with ability to fold/unfold nodes;
    * with the support of longer labels;

## Demo

* Run demo with `sh serve.sh` or, if you are using Nix OS, `sh ./nix-serve.sh` in `nix develop`;
* Select the example using the hash in the URL:
    `#init` - initial example;
    `#many` - many items example (more than 1.000?);
    `#simple` - simple example;
    `#letnum` - letters & numbers: a varierty of letter variants for latin alphabet;
