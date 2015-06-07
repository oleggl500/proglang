const W=100; // ширина поля
const H=100; // высота поля
const CellSize=25; // размер ячейки
const PossibleSigns=["X","O","I","S","V","Z"];
const ApiURLBase="/api/game_http_server/";
const WhoPlaysURL=ApiURLBase+"who_plays";
const JoinNewPlayerURL=ApiURLBase+"join";
const LeaveGameURL=ApiURLBase+"leave";
const GetFieldURL=ApiURLBase+"get_field";
const MakeTurnURL=ApiURLBase+"make_turn";
const ResetURL=ApiURLBase+"reset";
const WhoWonURL=ApiURLBase+"who_won";

$(document).ready(function()
{
    var playersData = []; // массив структур с полями name, symbol
    function resetGame() //reset
    {
        $.ajax({
            url:ResetURL,
            dataType:"text"
        }).done(function(status){
            if(status == "ok")
            {
                $("#join-leave-button").removeAttr("in_game");
                $("#join-leave-button").text("Присоединиться");
                $("#player-name").prop( "disabled", false );
                $("#who-plays").html("");
                $("#player-name").val("");
                createField();
                makePlayersList()
                setInterval(makePlayersList,1000);

                setInterval(isGameOver,1000);

            }
        })
    }

    function createField()// создание поля
    {
        var field = $("#field");
        field.html("");
        for(var i = 0; i < H; ++i)
        {
            for(var j = 0; j < W; ++j)
            {
                var top = CellSize*i;
                var left = CellSize*j;
                var square = "<div class='cell' "
                square += "x=" + j.toString() + " ";
                square += "y=" + i.toString() + " ";
                square += "style='top:" + top.toString() + "px;";
                square += "left:" + left.toString() + "px;";
                square += "width:" + CellSize.toString() + "px;";
                square += "height:" + CellSize.toString() + "px'/>";
                field.append(square)
            }
        }
        $(".cell").click(function(){
            var name = $("#player-name").val();
            var x = $(this).attr("x");
            var y = $(this).attr("y");
            tryMakeTurn(name, x, y)
        })
    }

    //принимает какой-то индекс, возвращает знак, которым соответствует индексу или некое число,
    //если знака нет в PossibleSigns
    function signByIndex(index)
    {
        if(index >= PossibleSigns.length)
        {
            return"" + (1+index-PossibleSigns.length)
        }
        else
        {
            return PossibleSigns[index]
        }
    }

    function signByName(name) //по имени возвращает символ
    {
        for(var i = 0; i < playersData.length; ++i)
        {
            if(playersData[i].name == name)
            {
                return playersData[i].symbol
            }
        }
        return""
    }


    function tryMakeTurn(name, x, y)//попытка сделать ход игроком name в точку x, y = y
    {
        $.ajax({
            url:MakeTurnURL + "/" + name + "/" + x + "/" + y,
            dataType:"text"
        }).done(function(status){
                if(status == "ok")
                {
                    makePlayersList()
                }
                else
                {
                    if(status == "not_your_turn")
                    {
                        alert("Сейчас не Ваш ход!")
                    }
                    else
                    {
                        if(status == "game_over_you_win")
                        {
                            $("#game-over-dialog").text("Поздравляем! Вы выиграли!").dialog("open")
                        }
                        else
                        {
                            if(status == "game_over")
                            {
                                alert("Игра окончена!")
                            }
                        }
                    }
                }
            })
    }

    function makePlayersList()// заполняет таблицу игроков и выделяет активного
    {
        $.ajax({
            url:WhoPlaysURL,
            dataType:"json"
        }).done(function(res){
            var playersList = res.players;
            var activePlayer = res.whose_turn;
            var playerHtml = "";
            playersData = [];
            for(var n=0; n < playersList.length; ++n)
            {
                var plName = playersList[n];
                var sign = signByIndex(n);
                playersData.push({symbol:sign,name:plName});
                if(plName == activePlayer)
                {
                    playerHtml += "<span class='player-name active'>" + plName + "(" + sign + ")</span>"
                }
                else
                {
                    playerHtml += "<span class='player-name inactive'>" + plName + "(" + sign + ")</span>"
                }
            }
            $("#who-plays").html(playerHtml);
            plotAllTurns()
        })
    }

    function plotAllTurns()// отмечатет в клеточках кто куда сходил
    {
        $.ajax({
            url:GetFieldURL,
            dataType:"json"
        }).done(function(res){
            for(var i = 0; i < res.length; ++i)
            {
                var sign=signByName(res[i].player);// символ игрока
                var squareHtml = ".cell[x='" + res[i].x + "'][y='" + res[i].y + "']";
                $(squareHtml).text(sign)
            }
        })//.fail(function(m,l,sign){var n=123})
    }

    function addPlayer() // добавить нового игрока
    {
        var name = $("#player-name").val(); // имя из input
        if(name.length)
        {
            $.ajax({
                url:JoinNewPlayerURL+"/" + name,
                dataType:"text"
            }).done(function(status){
                if(status == "ok")
                {
                    $("#join-leave-button").attr("in_game",true).text("Выйти");
                    makePlayersList()
					$("#player-name").prop( "disabled", true );
                }
                else
                {
                    if(status == "player_exists")
                    {
                        alert("Игрок с таким именем уже в игре")
                    }
                }
            })
        }
    }

    function removePlayer()// удаляет игрока из игры
    {
        var name=$("#player-name").val();
        if(name.length)
        {
            $.ajax({
                url:LeaveGameURL+"/"+name,
                dataType:"text"
            }).done(function(status){
                if(status == "ok")
                {
                    $("#join-leave-button").removeAttr("in_game").text("Присоединиться");
                    makePlayersList()
					$("#player-name").prop( "disabled", false );
                }
            })
        }
    }

    function isGameOver() // проверка на конец игры
    {
        $.ajax({
            url:WhoWonURL,
            dataType:"json"
        }).done(function(res){
            res = res.value;
            var name=$("#player-name").val();

            if(res)
            {
                if(name != res)
                {
                    $("#game-over-dialog").text("Поражение!Победил "+res+".").dialog("open")
                }
                else
                {
                    $("#game-over-dialog").text("Победа!Победил "+res+".").dialog("open")
                }
            }
            else
            {
                setTimeout(isGameOver,1000)
            }
        })
    }

    $("#join-leave-button").click(function(){
            var tf=$(this).attr("in_game");
            if(!tf)
            {
                addPlayer()
            }
            else{
                removePlayer()
        }});
    $("#reset-button").click(resetGame);
    createField();
    setInterval(makePlayersList,1000);

    setTimeout(isGameOver,1000);
    $("#game-over-dialog").dialog({
        title:"Игра окончена",
        modal:true,
        autoOpen:false,
        buttons:[{text:"Начать заново",click:function(){
            resetGame();
            $(this).dialog("close")
        }}]
    })
});
