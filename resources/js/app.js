(function() {
    up.compiler(".toggleable__toggle", function(el) {
        el.addEventListener("click", function(e) {
            var parentEl = e.target.closest(".toggleable");
            parentEl.classList.remove("toggleable--hidden");
        });
    });
})();
