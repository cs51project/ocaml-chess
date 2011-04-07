function load_board(b)
{
    var board = document.getElementById("board");
    
    var html = "";
    for(var rank = 7; rank >= 0; rank--)
    {
        html += "<tr id='rank_" + (rank + 1) + "'>";
        
        for(var file = 0; file < 8; file++)
        {
            var fileName = String.fromCharCode(65 + file);
            var background = ((file % 2) ^ (rank % 2))? "white" : "black";
            var color = (background === "black")? "white" : "black";
            var id = fileName + (rank + 1);
            
            html += "<td class='file_" + fileName + "' id='" + id +
                    "' style='background-color: " + background +
                    "; color: " + color + ";'>";
            
            //html += id;
            html += "<div class='piece'></div>";
            
            html += "</td>";
        }
        
        html += "</tr>";
    }
    
    board.innerHTML = html;
}