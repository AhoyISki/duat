// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><a href="introduction.html">Introduction</a></li><li class="chapter-item expanded affix "><li class="part-title">Initial setup</li><li class="chapter-item expanded "><a href="installation.html"><strong aria-hidden="true">1.</strong> Installation</a></li><li class="chapter-item expanded "><a href="quick-settings/chapter.html"><strong aria-hidden="true">2.</strong> Quick settings</a><a class="toggle"><div>❱</div></a></li><li><ol class="section"><li class="chapter-item "><a href="quick-settings/setup-and-setup-duat.html"><strong aria-hidden="true">2.1.</strong> The setup function and setup_duat!</a></li><li class="chapter-item "><a href="quick-settings/prelude/chapter.html"><strong aria-hidden="true">2.2.</strong> The prelude module</a><a class="toggle"><div>❱</div></a></li><li><ol class="section"><li class="chapter-item "><a href="quick-settings/prelude/print-module.html"><strong aria-hidden="true">2.2.1.</strong> print: How duat should print files</a></li><li class="chapter-item "><a href="quick-settings/prelude/form-module.html"><strong aria-hidden="true">2.2.2.</strong> form: How text is colored</a></li><li class="chapter-item "><a href="quick-settings/prelude/map-and-alias.html"><strong aria-hidden="true">2.2.3.</strong> map and alias: modifying keys</a></li><li class="chapter-item "><a href="quick-settings/prelude/cursor-module.html"><strong aria-hidden="true">2.2.4.</strong> cursor: How to print cursors</a></li></ol></li><li class="chapter-item "><a href="quick-settings/list-of-forms.html"><strong aria-hidden="true">2.3.</strong> List of forms</a></li><li class="chapter-item "><a href="quick-settings/frequent-snippets/chapter.html"><strong aria-hidden="true">2.4.</strong> Frequently used snippets</a><a class="toggle"><div>❱</div></a></li><li><ol class="section"><li class="chapter-item "><a href="quick-settings/frequent-snippets/map-jk.html"><strong aria-hidden="true">2.4.1.</strong> mapping jk and others to esc</a></li><li class="chapter-item "><a href="quick-settings/frequent-snippets/status-per-file.html"><strong aria-hidden="true">2.4.2.</strong> StatusLine on each File</a></li><li class="chapter-item "><a href="quick-settings/frequent-snippets/prompt-status-line.html"><strong aria-hidden="true">2.4.3.</strong> Prompt and Status on same line</a></li><li class="chapter-item "><a href="quick-settings/frequent-snippets/common-status.html"><strong aria-hidden="true">2.4.4.</strong> Common StatusLine parts</a></li><li class="chapter-item "><a href="quick-settings/frequent-snippets/rel-align-linenums.html"><strong aria-hidden="true">2.4.5.</strong> Relative and aligned LineNumbers</a></li><li class="chapter-item "><a href="quick-settings/frequent-snippets/file-tabstops.html"><strong aria-hidden="true">2.4.6.</strong> File wise tabstops</a></li><li class="chapter-item "><a href="quick-settings/frequent-snippets/nerd-status.html"><strong aria-hidden="true">2.4.7.</strong> Nerdfonts StatusLine</a></li><li class="chapter-item "><a href="quick-settings/frequent-snippets/file-window-status.html"><strong aria-hidden="true">2.4.8.</strong> Status on files and on window</a></li></ol></li></ol></li><li class="chapter-item expanded "><a href="scripting-duat/chapter.html"><strong aria-hidden="true">3.</strong> Scripting Duat</a><a class="toggle"><div>❱</div></a></li><li><ol class="section"><li class="chapter-item "><a href="scripting-duat/pass.html"><strong aria-hidden="true">3.1.</strong> The Pass and Duat&#39;s global state</a></li><li class="chapter-item "><a href="scripting-duat/hook-module.html"><strong aria-hidden="true">3.2.</strong> The hook module</a></li><li class="chapter-item "><a href="scripting-duat/text/chapter.html"><strong aria-hidden="true">3.3.</strong> The Text struct</a><a class="toggle"><div>❱</div></a></li><li><ol class="section"><li class="chapter-item "><a href="scripting-duat/text/builder-and-txt.html"><strong aria-hidden="true">3.3.1.</strong> Builder and txt!</a></li><li class="chapter-item "><a href="scripting-duat/text/tags.html"><strong aria-hidden="true">3.3.2.</strong> Tags: manipulating Text</a></li></ol></li><li class="chapter-item "><a href="scripting-duat/mod-status.html"><strong aria-hidden="true">3.4.</strong> Modding the StatusLine</a></li><li class="chapter-item "><a href="scripting-duat/context-module.html"><strong aria-hidden="true">3.5.</strong> The context module</a></li><li class="chapter-item "><a href="scripting-duat/cmd-module.html"><strong aria-hidden="true">3.6.</strong> cmd: Runtime commands</a></li><li class="chapter-item "><a href="scripting-duat/mod-layout.html"><strong aria-hidden="true">3.7.</strong> Modifying the layout</a></li></ol></li><li class="chapter-item expanded "><a href="extending-duat/chapter.html"><strong aria-hidden="true">4.</strong> Extending Duat</a><a class="toggle"><div>❱</div></a></li><li><ol class="section"><li class="chapter-item "><a href="extending-duat/duat-core.html"><strong aria-hidden="true">4.1.</strong> duat-core</a></li><li class="chapter-item "><a href="extending-duat/plugins.html"><strong aria-hidden="true">4.2.</strong> Plugins</a></li><li class="chapter-item "><a href="extending-duat/widgets.html"><strong aria-hidden="true">4.3.</strong> Widgets</a></li><li class="chapter-item "><a href="extending-duat/modes.html"><strong aria-hidden="true">4.4.</strong> Modes</a></li><li class="chapter-item "><a href="extending-duat/parsers.html"><strong aria-hidden="true">4.5.</strong> Parsers</a></li></ol></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0].split("?")[0];
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
