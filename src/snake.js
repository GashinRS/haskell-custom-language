function startGame(){
    call setPlayerStart(0,0);
    return 0;
}

function moveDown(){
    height = (call getHeight()/2);
    if((call getPlayerY() > -height)){
        call updatePlayerPos(0, -1);
    }
    return 0;
}

function moveUp(){
    height = (call getHeight()/2);
    if((call getPlayerY() < height)){
        call updatePlayerPos(0, 1);
    }
    return 0;
}

function moveLeft(){
    width = (call getWidth()/2);
    if((call getPlayerX() > -width)){
        call updatePlayerPos(-1,0);
    }
    return 0;
}

function moveRight(){
    width = (call getWidth()/2);
    if((call getPlayerX() < width)){
        call updatePlayerPos(1,0);
    }
    return 0;
}