:- module(http_utils, 
        [
          http_load_html/2
        ]).

http_load_html(URL, DOM) :-
    setup_call_cleanup(http_open(URL, In,
       [ timeout(10)
       ]),
       (   dtd(html, DTD),
           load_structure(stream(In),
                          DOM,
                          [ dtd(DTD),
                            dialect(sgml),
                            shorttag(false),
                            max_errors(-1),
                            syntax_errors(quiet)
                          ])
       ),
       close(In)).