-module(db).
-compile(export_all).
-author('mafraba@gmail.com').

%% Creates new DB
new() -> [].

%% Destroys DB
destroy(_) -> ok.

%% Adds element into DB (supposing duplicates are allowed)
write(Key, Element, Db) -> [{Key, Element} | Db].

%% Deletes element from Db
delete(_, []) -> [];
delete(Key, [{Key,_}|T]) -> delete(Key, T);
delete(Key, [H|T]) -> [H|delete(Key,T)].

%% Finds a key (if existing)
read(_,[]) -> {error, instance};
read(Key, [{Key,Element}|_]) -> {ok, Element};
read(Key, [_|T]) -> read(Key,T).

%% Finds keys for an element
match(_,[]) -> [];
match(Element, [{Key, Element}|T]) -> [Key|match(Element,T)];
match(Element, [_|T]) -> match(Element,T).

