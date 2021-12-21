width = (call getWidth()/2);
height = (call getHeight()/2);

function startGame(){
    call setPlayerStart(0,0);
    call addRandomTarget();
    return 0;
}

function nextStep(){
    playerTilesBefore = call getUniquePlayerTiles();
    call movePlayer();
    playerTilesAfter = call getUniquePlayerTiles();

    x = call getPlayerX();
    y = call getPlayerY();
    if ((call targetAt(x,y) == 1)){
        call removeAllTargets();
        call addRandomTarget();
        call lengthenPlayer();
    }

    isInBound = call inBound(x, y, width, height);
    if (((isInBound != 1) || (playerTilesAfter != playerTilesBefore))){
        call setGameOver();
    }
    return 0;
}

function moveDown(){
    if ((call getDirectionY() != 1)){
        call setDirection(0, -1);
    }
    return 0;
}

function moveUp(){
    if ((call getDirectionY() != -1)){
        call setDirection(0, 1);
    }
    return 0;
}

function moveLeft(){
    if ((call getDirectionX() != 1)){
        call setDirection(-1, 0);
    }
    return 0;
}

function moveRight(){
    if ((call getDirectionX() != -1)){
        call setDirection(1, 0);
    }
    return 0;
}

function pressSpace(){
    return 0;
}