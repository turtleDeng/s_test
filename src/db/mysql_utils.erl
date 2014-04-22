-module (mysql_utils).
-export ([save/4, find/6]).

-include_lib("./../deps/emysql/include/emysql.hrl").

find(ConnPool, Tab, Return, Conditions, Rec, Fields) ->
    find(ConnPool, Tab, Return, Conditions, undefined, Rec, Fields).

find(ConnPool, Tab, Return, Conditions, OrderBy, Rec, Fields)  ->
    {FindSql, CondVals} = build_find_sql(Tab, Conditions, OrderBy, Return),
    Result = emysql:execute(ConnPool, FindSql, CondVals),
    Returning = emysql_util:as_record(Result, Rec, Fields),
    case Returning of
        [] -> case Return of
                all -> [];
                _   -> undefined
            end;
        _  ->
            case [Return, Returning] of
                [_,   undefined]        -> undefined;
                [all,         _]        -> Returning;
                [first, [Return0| _T] ] -> Return0
            end
    end.    

save(ConnPool, Tab, Rec, Fields) ->
    save(ConnPool, Tab, Rec, Fields, []).

save(ConnPool, Tab, Rec, Fields, Options) ->
    [SqlType, UpdateVals, UpdateSql] = build_save_sql(Tab, Rec, Fields, Options),
    case emysql:execute(ConnPool, UpdateSql, UpdateVals) of
        #ok_packet{affected_rows = _Affected, insert_id = InsertId} ->
            case SqlType of
                insert_sql ->
                    case lists:nth(1, Fields) =:= id of
                        true -> setelement(2, Rec, InsertId);
                        false -> Rec
                    end;
                _ -> Rec
            end;
        #error_packet{code=Code, msg=Msg}  -> 
            io:format("Code:~p~n, Msg:~p~n", [Code, Msg]),
        throw({Code, Msg})
    end.


build_save_sql(Tab, Rec, Fields, Options) ->
 %    {SqlType, Head, Tail} = case element(2, Rec) of
 %                                undefined -> % new record in database
 %                                    {insert_sql, "insert into ", ""};
 %                                _ ->
 %                                    PK = atom_to_list(lists:nth(1, Fields)),
 %                                    {update_sql, "update ", " where " ++ PK ++ " = ?"}
 %                            end,
	% io:format("Rec~p~n", [Rec]),
	{SqlType, Head, Tail} = {insert_sql, "insert into ", ""},                           
    {_, UpdateStmt, UpdateValues} =
        lists:foldl(
          fun(Field, {Index, Stmt, Vals}) ->
                  case {Index, SqlType} of
                      {1, update_sql} ->
                          {Index + 1, Stmt, Vals };
                      _ ->
                          case proplists:get_value(only, Options) of
                              undefined ->
                                  SetStmt = "`" ++ atom_to_list(Field) ++ "` = ? ",
                                  Val = element(Index + 1, Rec),
                                  {Index + 1, [SetStmt | Stmt],  [Val | Vals] };
                              OnlyUpdateFields ->
                                  case lists:memeber(Field, OnlyUpdateFields) of
                                      true ->
                                          SetStmt = "`" ++ atom_to_list(Field) ++ "` = ? ",
                                          Val = element(Index + 1, Rec),
                                          {Index + 1, [SetStmt | Stmt], [Val | Vals] };
                                      false ->
                                          {Index + 1, Stmt, Vals }
                                  end
                              end
                  end
          end, {1, [], []}, Fields),

    UpdateVals = case SqlType of
                     insert_sql -> UpdateValues;
                     update_sql -> UpdateValues ++ [element(2, Rec)]
                 end,
    UpdateSql = list_to_binary([Head, atom_to_list(Tab), " set " ++ string:join(UpdateStmt, ","), Tail]),
    [SqlType, UpdateVals, UpdateSql].    

build_find_sql(Tab, Conditions, OrderBy, Return) ->
    Select = "select * from " ++ atom_to_list(Tab),
    {Where, _, CondVals} =
        case Conditions of
            [] -> {"", 0, []};
            _  -> lists:foldl(
                    fun(Condition, {Sql, Idx, Vals}) ->
                            NSql = case Idx of
                                       0 -> Sql ++ parameterize(Condition);
                                       _ -> Sql ++ " and " ++ parameterize(Condition)
                                   end,
                            NVals = concat_value(Vals, Condition),
                            NIdx  = Idx + 1,
                            {NSql, NIdx, NVals}
                    end, {" where ", 0, []}, Conditions)
        end,
    FindSql = Select ++ Where,
    FindSql2 = case OrderBy of
                   undefined -> FindSql;
                   _ -> FindSql ++ " order by " ++ OrderBy
               end,
    FindSql3 = case Return of
                   all   -> FindSql2;
                   first -> FindSql2 ++ " limit 1"
               end,
    
    {FindSql3, CondVals}.

parameterize({Field, between, [_R1, _R2]}) ->
    atom_to_list(Field) ++ " between ? and ? ";
parameterize({Field, Op, _Val}) ->
    atom_to_list(Field) ++ " " ++ atom_to_list(Op) ++ " ? ";
parameterize({Field, _Val}) ->
    atom_to_list(Field) ++ " = ? ".

concat_value(ValList, {_F, between, [R1, R2]}) ->
    lists:append(ValList, [R1, R2]);
concat_value(ValList, {_F, _Op, V}) ->
    lists:append(ValList, [V]);
concat_value(ValList, {_F, V}) ->
    lists:append(ValList, [V]).    