:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module('string_utils.pl').
:- use_module('http_utils.pl').
:- use_module('parse_utils.pl').
:- use_module('homepage.pl').

:- http_handler(/, say_hi, []).
:- http_handler('/submit', submit, []).
:- http_handler('/kth_login_callback/', kth_login_callback, [prefix]).
:- http_handler('/setlang/', setlang, [prefix]).


setlang([path_info(en)|_]):-
        http_session_retractall(lang(_)),
    http_session_assert(lang(en)),!,
    http_session_id(Sid),
    http_redirect(see_other, /, T).
setlang([path_info(se)|_]):-
    http_session_retractall(lang(_)),
    http_session_assert(lang(se)),
    http_redirect(see_other, /, _).
setlang([path_info(Lang)|_]):-
    reply_html_page([], p([Lang, ' is not supported'])).

getlang(Lang):-
    http_session_data(lang(Lang)),!.
getlang(se):-
    http_session_assert(lang(se)).

uid([H|T]) -->
    [H],
    {
        \+code_type(H, quote)
    },
    uid(T).
uid([]) --> [].
loginverify(Uid)-->
    "{\"user\":\"", uid(LUid),"\"}", 
    {
        string_to_list(Uid, LUid)
    }.

cleanname([H])--> [H].
cleanname([H|T])--> [H], cleanname(T).


dirtyname(Name)-->"\t\t", cleanname(Name),"\t\t", anything.

kth_login_callback([path_info(Atoken)|_Request]):-
        string_to_atom(Stoken, Atoken),
        ['http://login.datasektionen.se/verify/', Stoken, '.json'] squashTo VerifyAddrs,
        http_get(VerifyAddrs, Result, []),
        string_to_list(Result, LResult),
        phrase(loginverify(Uid), LResult),
        ['http://www.csc.kth.se/hacks/new/xfinger/results.php?freetext=', Uid] squashTo AddresToFinger,
    string_to_atom(AddresToFinger, SAddres),
    http_load_html(SAddres, DOM),
    xpath(DOM, //tr(text), DirtyName),
    string_to_list(DirtyName, LDirtyName),
    phrase(dirtyname(LName), LDirtyName),
    string_to_list(Name, LName),
    xpath(DOM, //img(@src), Img),
    http_session_assert(name(Name)),
    http_session_assert(uid(Uid)),
    http_session_assert(img(Img)),
    http_redirect(see_other, /, _Request).

kth_login(PublicUrl)-->
    html(b(
        a([href=['http://login.datasektionen.se/login?callback='+
            PublicUrl+location_by_id(kth_login_callback)]],
            'kth login'))).

list([H])-->
    item(H).
list([H|T])-->
    item(H),
    list(T).
list([])-->
    html('').
item([_=I, _=T, _=N, _=Img])-->
    html([div([class='shadow', style='padding: 30px 15px; margin: 10px auto; width: 40%; background-color: white;'], 
        [img([src='http://www.csc.kth.se/hacks/new/xfinger/'+Img, style='float:left;width:60px; height:100px; margin:10px;'],''), h3(N),p(I), div(style='clear:both;',p(em(T)))])]).

float_div(Dir, Centent)-->
    html(div(style='float:'+Dir, Centent)).
clear_div(Content)-->
    html(div(style='clear:left', Content)).
padding_div(Pad, Content)-->
    html(div(style='padding:'+Pad), Content).
lang_button(Lang)-->
    html(a(href=location_by_id(setlang)+Lang, Lang)).

footr-->
    html(center(p(style='color:darkgray','Created Daniil by Pintjuk, Maintained by Ior #2015'))).

submit(_Request) :-
    http_parameters(_Request, [],[form_data(Fdata)]),
    http_session_data(name(Name)),
    http_session_data(img(Img)),
    nth1(_, Fdata, 'en'=E),
    nth1(_, Fdata, 'se'=S),
    get_time(T),
    asserta(messages([en=E, se=S],T, Name, Img)),
    http_redirect(see_other, /, _Request).

messages_in_lang(Lang, Messages):-
    findall(X, (messages(Z, T, N, I), nth1(_, Z, Lang=M), X = [text=M, time=T, name=N, img=I]), Messages).

main :-
    asserta(messages([], '', '','')),
    server(8080).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).




