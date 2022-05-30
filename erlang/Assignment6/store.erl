%% Distributed purchasing system

-module(store).
-export([start/0,  
        close_store/0, open_store/0, list_partners/0, sold_products/0,
        subscribe_partner/1, delete_partner/1, create_order/2,
        register_product/2, remove_product/1, modify_stock/2, stock_list/1, product/3,
        test/0]).

% transforms an atom name to a short machine name
getNode(Name) -> list_to_atom(
                    atom_to_list(Name)++"@Sebastians-MacBook-Air").

% Creates and starts the master process, as well as 
% registering it with the 'store' name.
start() ->
    register(store, spawn(?MODULE, open_store, [])),
    io:format("store created~n").
    
%% ==============================================================
%% Store Entity 

% Asks the Master store process to stop. Before terminating, 
% it should eliminate all its slave processes by 
% sending them die messages .
close_store() ->
    {store, getNode(store)} ! stop.
    
% function to create the store as a master process
open_store() -> open_store(0, [], [], []).
open_store(Counter, Partners, ProductsPids, Orders) ->
    receive
        {create_product, Product, Quantity} ->
            ProductsNode = getNode(products),
            Alive = net_adm:ping(ProductsNode),
            if
                Alive == pang ->
                    io:format("node products is down~n"),
                    open_store(Counter, Partners, ProductsPids, Orders);
                true ->
                    Pid = spawn(ProductsNode, ?MODULE, product, [store, Product, Quantity]),
                    io:format("Pid ~p ~n", [Pid]),
                    case rpc:call(ProductsNode, erlang, is_process_alive, [Pid]) of
                        true -> 
                            io:format("~p slave ~p created in node ~p~n", [store, Product, ProductsNode]),
                            open_store(Counter, Partners, ProductsPids++[{Product, Pid}], Orders);
                        false -> 
                            io:format("node ~p does not exist~n", [ProductsNode]),
                            open_store(Counter, Partners, ProductsPids, Orders)
                    end
            end;
        {partner_msg, Message, Partner} ->
            case Message of 
                subscribe_partner ->
                    PartnerExists = lists:member(Partner, Partners),
                    case PartnerExists of  
                        true ->
                            io:format("Partner already exists, please try with a different name ~n"),
                            open_store(Counter, Partners, ProductsPids, Orders);
                        false ->
                            io:format("Creating partner ~n"),
                            open_store(Counter, Partners++[Partner], ProductsPids, Orders)
                    end;
                delete_partner ->
                    PartnerExists = lists:member(Partner, Partners),
                    case PartnerExists of
                        true -> 
                            NewPartnerList = lists:delete(Partner, Partners),
                            io:format("Deleting partner ~p ~n", [Partner]),
                            open_store(Counter, NewPartnerList, ProductsPids, Orders);
                        false ->
                            io:format("Partner ~p is not registered ~n", [Partner]),
                            open_store(Counter, Partners, ProductsPids, Orders)
                    end;
                {create_order, Partner, ProductList} ->
                    NewCounter = Counter + 1,
                    io:format("Creating order for partner ~p ~n", [Partner]),
                    open_store(NewCounter, Partners, ProductsPids, Orders++[{NewCounter, create_order(Partner, ProductList, [])}]);
                true ->
                    io:format("message unrecognized"),
                    open_store(Counter, Partner, ProductsPids, Orders)
            end;
        {product_msg, Message, ProductName} -> 
            {_ , {_ , Pid}} = lists:search(fun({Product, _}) -> Product == ProductName end, ProductsPids), 
            io:format("Found PID ~p ~n", [Pid]),
            case rpc:call(node(Pid), erlang, is_process_alive,[Pid]) of
                true ->
                    if
                        Message == die ->
                            Pid ! {msg, Message},
                            NewProductsPids = lists:delete(Pid, ProductsPids),
                            open_store(Counter, Partners, NewProductsPids, Orders);
                        true ->
                            Pid ! {msg, Message},
                            open_store(Counter, Partners, ProductsPids, Orders)
                    end;
                false ->
                    io:format("~p slave ~p does not exist~n",[store, ProductName]),
                    open_store(Counter, Partners, ProductsPids, Orders)
            end;
        {msg, list_partners} ->
            io:format("Partner List: ~n"),
            list_partners(Partners),
            open_store(Counter, Partners, ProductsPids, Orders);
        {msg, sold_products} ->
            sold_products(Orders),
            open_store(Counter, Partners, ProductsPids, Orders);
        stop -> 
            kill_all(ProductsPids),
            io:format("master ~p has finished~n",[store])
    end.

    
% Helper functions for termination of the master process
kill_all([{_ , SlavePid} | Rest]) ->
    case rpc:call(node(SlavePid), erlang, is_process_alive,[SlavePid]) of
        true -> 
            SlavePid ! {msg, die},
            kill_all(Rest);
        false ->
            kill_all(Rest)
    end;
kill_all([]) -> bye.

% Tells master process to execute this function with Partner List to initiate recursive calls
list_partners() -> 
    {store, getNode(store)} ! {msg, list_partners}.
% Recursively displays a list of partners
list_partners([]) ->
    io:format("END OF PARTNER LIST ~n");
list_partners([Partner | RestPartners]) ->
    io:format("~p ~n", [Partner]),
    list_partners(RestPartners).

% Tells master process to execute this function with Order List to initiate recursive calls
sold_products() ->
    {store, getNode(store)} ! {msg, sold_products}.
% Recursiveley displays a list of orders 
sold_products([]) ->
    undefined;
sold_products([{OrderNumber, {Partner, OrderList}}, RestOfOrders]) ->
    io:format("Order history: ~n"),
    io:format("--- Order #~p ---~n", [OrderNumber]),
    io:format("Ordered by ~p ~n", [Partner]),
    list_order(OrderList),
    sold_products(RestOfOrders).

% Helper function to list all products in one order
list_order([]) -> undefined;
list_order([{Product, QuantityOrdered} | RestOrderList]) ->
    io:format("~p ~p ~n", [Product, QuantityOrdered]),
    list_order(RestOrderList).

%% ==============================================================
% Partners entity
subscribe_partner(Partner) -> 
    send_partner_msg(subscribe_partner, Partner).

delete_partner(Partner) ->
    send_partner_msg(delete_partner, Partner).

create_order(Partner, ProductList) -> 
    
    create_order(Partner, ProductList, []).
create_order(Partner, [ ], OrderList) -> {Partner, OrderList};
create_order(Partner, [{Product, QuantityOrdered} | RestProductList], OrderList) ->
    CurrentStock = send_product_msg(get_stock, Product),
    if 
        CurrentStock < QuantityOrdered ->
            io:format("~p is now all out of stock", [Product]),
            create_order(Partner, RestProductList, OrderList++[{Product, CurrentStock}]);
        true ->
            register_product(Product, CurrentStock - QuantityOrdered), % reregister product with modified quantity
            create_order(Partner, RestProductList, OrderList++[{Product, QuantityOrdered}])
    end.
            

% forwards a message from the partner entity to the master process
send_partner_msg(Message, Partner) ->
    {store, getNode(store)} ! {partner_msg, Message, Partner}.

%% ==============================================================
% Products entity
% tells master store process to create a slave process to track the product with its quantity
register_product(Product, Quantity) ->
    {store, getNode(store)} ! {create_product, Product, Quantity}.

% tells master process to search for a product process and remove it
remove_product(Product) -> 
    io:format("Removing product ~p ~n", [Product]),
    send_product_msg(die, Product).

% tells master process to find a slave process and modify the stock for the product it is tracking
modify_stock(Product, Quantity) -> 
    send_product_msg({modify_stock, Quantity}, Product).

% tells master process to show the stock list for a specific product
stock_list([]) -> 
    io:format("Stock List: ~n");
stock_list([Product | RestProducts]) -> 
    send_product_msg(show_stock, Product),
    stock_list(RestProducts).

% function to create products as slave processes
product(Master, ProductName, Quantity) -> 
    receive
        {msg, die} -> 
            io:format(user, "~p process for product ~p has died~n",
                      [Master, ProductName]);
        {msg, Message} ->
            io:format(user, "~p process for product ~p received msg: ~p ~n",
                      [Master, ProductName, Message]),
            case Message of
                {modify_stock, NewQuantity} -> 
                    io:format("Modifying ~p stock to ~p ~n", [ProductName, NewQuantity]),
                    product(Master, ProductName, NewQuantity);
                show_stock ->
                    io:format("~p -> ~p ~n", [ProductName, Quantity]),
                    product(Master, ProductName, Quantity);
                get_stock ->
                    io:format("Returning stock quantity ~p ~n", [Quantity]),
                    Quantity;
                true ->
                    product(Master, ProductName, Quantity)
            end;
        true ->
            product(Master, ProductName, Quantity)
    end.

% forwards a message from the product entity to the master process
send_product_msg(Message, ProductName) ->
    {store, getNode(store)} ! {product_msg, Message, ProductName}.

%% ==============================================================

%% Test cases
% To test this prototype, first open to terminals
% On the first one, run the command erl -sname store
% On the second one, run the command erl -sname products
% This will enable the two distributed nodes on which the system runs
test() -> 
    start(),
    register_product(apple, 30),
    register_product(orange, 25),
    register_product(pear, 28),
    subscribe_partner(sebas),
    subscribe_partner(ana),
    subscribe_partner(estefania),
    subscribe_partner(juan),
    list_partners(),
    delete_partner(estefania),
    list_partners(),
    stock_list([apple, orange, pear]),
    create_order(sebas, [{apple, 3}, {orange, 25}]),
    stock_list([apple, orange]),
    modify_stock(orange, 4),
    stock_list([orange]),
    remove_product(pear),
    stock_list([apple, orange, pear]),
    create_order(ana, [{apple, 2}, {orange, 2}]).
