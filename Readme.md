# Café Analog Indicator for Emacs #

The `analog-indicator-mode` displays a little 🍵 in the Emacs mode line whenever [Café Analog](http://cafeanalog.dk) at the IT University of Copenhagen is open.

If you spend your entire working day in Emacs anyway, why bother checking on their web site? This nifty minor mode simply queries the Analog web API ~~as often as you'd like~~ every ten minutes or so ~~and even tells you when Analog is about to close~~.

This will hopefully become a useful tool for ITU students and employees.

## Planned Features ##

- [x] Interval based updates of opening status.
- [ ] Three-level indicator in Emacs' mode line: Open, Closed, Closes soon.
- [ ] Customizable frequency of status checks.
