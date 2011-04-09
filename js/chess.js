// AJAX request object
var xhr = null;
// asynchronicity parameter
var async = true;

/* Comprehensive board object -- stores the state of a game
 * on the client side.  We can build a board from a string in
 * Forsyth-Edwards Notation.
 * (See http://en.wikipedia.org/wiki/ForsythÐEdwards_Notation.)
 */
function Board(strFEN)
{
    // first split FEN string into segments
    var sgt = strFEN.split(" ");
    
    // parse the piece positions
    this.pieces = new Array();
    
    var ranks = sgt[0].split("/");
    for(var i in ranks)
    {
        var rank = 7 - i;
        
        // helper function for parsing one rank of FEN
        function parseRank(str, file, buffer)
        {
            // if we have reached the end of the rank, return
            if(str === "" || file >= 8)
                return buffer;
            
            // if next char is a number, skip that number of squares
            var code = str.charCodeAt(0);
            if(code >= 48 && code < 58)
                return parseRank(str.substr(1), file + (code - 48), buffer);
                
            var p = String.fromCharCode(code);
            var color = (p.toLowerCase() === p)? "b" : "w";
            
            function fullName(letter)
            {
                switch(letter)
                {
                    case "p": return "pawn";
                    case "n": return "knight";
                    case "b": return "bishop";
                    case "r": return "rook";
                    case "q": return "queen";
                    case "k": return "king";
                }
            }
            
            var piece = color + fullName(p.toLowerCase());
            buffer[file] = piece;
            
            return parseRank(str.substr(1), file + 1, buffer);
        }
        
        this.pieces[rank] = parseRank(ranks[i], 0, new Array());
    }
    
    // parse other data:
    
    // which player is to move
    this.toMove = sgt[1];
    
    // which castles have been precluded
    var castling = sgt[2];
    this.wKingside = false;
    this.wQueenside = false;
    this.bKingside = false;
    this.bQueenside = false;
    for(var i in castling)
    {
        switch(castling[i])
        {
            case 'K': this.wKingside = true; break;
            case 'Q': this.wQueenside = true; break;
            case 'k': this.bKingside = true; break;
            case 'q': this.bQueenside = true; break;
        }
    }
    
    // current En Passant target
    this.epTarget = sgt[3];
    
    this.pieceAt = function(rank, file)
    {
        return this.pieces[rank][file];
    }
    
    this.toFEN = function()
    {
        function letter(fullName)
        {
            var color = fullName.charAt(0);
            fullName = fullName.substr(1);
            var letter = (fullName === "knight")? "n" : (fullName.charAt(0));
            return (color === "b")? letter : letter.toUpperCase();
        }
        
        // encode the board configuration
        var encoding = "";
        for(var rank = 7; rank >= 0; rank--)
        {
            var gap = 0;
            for(var file = 0; file < 8; file++)
            {
                var piece = this.pieceAt(rank, file);
                if(piece)
                {
                    if(gap > 0)
                        encoding += gap;
                    encoding += letter(piece);
                    gap = 0;
                }
                else gap++;
            }
            
            if(gap > 0)
                encoding += gap;
            
            if(rank > 0)
                encoding += "/";
        }
        
        // encode player to move
        encoding += " " + this.toMove + " ";
        
        // encode availability of castle moves
        var castleStatus = (this.wKingside? "K" : "") +
                           (this.wQueenside? "Q" : "") +
                           (this.bKingside? "k" : "") +
                           (this.bQueenside? "q" : "");
        if(castleStatus === "")
            castleStatus = "-";
        encoding += castleStatus + " ";
        
        // encode En Passant target
        encoding += this.epTarget;
        
        return encoding;
    }
}

// display a board
function loadBoard(b)
{
    var boardView = document.getElementById("board");
    
    var html = "";
    for(var rank = 7; rank >= 0; rank--)
    {
        html += "<tr id='rank_" + (rank + 1) + "'>";
        
        for(var file = 0; file < 8; file++)
        {
            // set up the chessboard pattern
            var fileName = String.fromCharCode(65 + file);
            var background = ((file % 2) ^ (rank % 2))? "white" : "#05A";
            var color = (background === "#05A")? "white" : "#05A";
            var id = fileName + (rank + 1);
            
            html += "<td class='file_" + fileName + "' id='" + id +
                    "' style='background-color: " + background +
                    "; color: " + color + ";'>";
            
            html += "<div class='piece' draggable=true>";
            
            // insert the proper piece into each square
            if(b != null && b.pieceAt(rank, file) != null)
            {
                var piece = b.pieceAt(rank, file);
                html += "<div class='piece." + piece +
                        "' style=\"background-position: center;" +
                        "background-image: url('images/" + piece +
                        ".png'); height: 45px; width: 45px;\"></div>";
            }
            
            html += "</div></td>";
        }
        
        html += "</tr>";
    }
    
    boardView.innerHTML = html;
}

function initBoard()
{
    loadBoard(new Board("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"));
}

// Send a request to the server via AJAX
function sendAJAX(params, callback)
{
    // initialize XHR
    try
    {
        xhr = new XMLHttpRequest();
    }
    catch (e)
    {
        xhr = new ActiveXObject("Microsoft.XMLHTTP");
    }
    
    if (xhr == null)
    {
        alert("Error: Your browser is not configured to support AJAX!");
        return;
    }
    
    // initialize callback function
    function genericAJAXHandler(f)
    {
        return function()
        {
            // only handle loaded requests
            if (xhr.readyState == 4)
            {
                if (xhr.status == 200)
                {
                    f(xhr.responseText)
                }
            }
            return;
        };
    }
    
    xhr.onreadystatechange = genericAJAXHandler(callback);
    
    xhr.open("POST", "index.html", async);
    
    // set HTTP headers (adapted from http://www.openjs.com/articles/ajax_xmlhttp_using_post.php)
    xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    
    // send form via POST
    xhr.send(params);
    return;
}

function urlEncode(str)
{
    return str.replace(/[ \n\t]+/g, "+");
}

function handleBoard(responseFEN)
{
    var board = new Board(responseFEN);
    loadBoard(board);
}

/* Submit a move to the server via AJAX.
 * Returns false if invalid else a new board.
 */
function submitMove(board, move)
{
    function moveCallback(response)
    {
        /* response is either "false," indicating invalid move,
         * or a new Board
         */
        if(response !== "false")
        {
            handleBoard(response);
        }
    }
    
    return sendAJAX("request=submit_move&board=" + urlEncode(board.toFEN()) +
                    "&move=" + urlEncode(move), moveCallback);
}

/* Request the board from the server.
 * If the server is to move, causes the
 * server to make its move.
 */
function requestMove(board)
{
    return sendAJAX("request=get_move&board=" + urlEncode(board.toFEN()), handleBoard);
}