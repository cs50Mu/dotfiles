// reason to add this: https://github.com/brookhong/Surfingkeys/issues/1574
// https://github.com/brookhong/Surfingkeys/wiki/Migrate-your-settings-from-0.9.74-to-1.0
const {
    aceVimMap,
    mapkey,
    imap,
    imapkey,
    getClickableElements,
    vmapkey,
    map,
    cmap,
    addSearchAlias,
    removeSearchAlias,
    tabOpenLink,
    readText,
    Clipboard,
    Front,
    Hints,
    Visual,
    RUNTIME
} = api;

// change tab
map('q', 'E');
map('w', 'R');

// go back and forth of the history
map('h', 'S');
map('l', 'D');

// close tab
map('d', 'x')

// restore tab
map('u', 'X')

// open a link from clipboard
map('p', 'cc');

// open a link in new tab
map('F', 'af');

// 切换回最近的tab
// 参考：https://github.com/brookhong/Surfingkeys/issues/949
map(";b", '<Ctrl-6>');

// change scroll step size
settings.scrollStepSize = 140;
// disable smooth scroll
settings.smoothScroll = false;


// set theme
settings.theme = `
.sk_theme {
    background: #000;
    color: #fff;
}
.sk_theme tbody {
    color: #fff;
}
.sk_theme input {
    color: #d9dce0;
}
.sk_theme .url {
    color: #2173c5;
}
.sk_theme .annotation {
    color: #38f;
}
.sk_theme .omnibar_highlight {
    color: #fbd60a;
}
.sk_theme ul>li:nth-child(odd) {
    background: #1e211d;
}
.sk_theme ul>li.focused {
    background: #4ec10d;
}`;
// click `Save` button to make above settings to take effect.
