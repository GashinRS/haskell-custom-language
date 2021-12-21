function startGame(){
    bottom = (-1 * (call getHeight()/2));
    call setPlayerStart(0,bottom);
    call addTarget(0,10);
    call addTarget(1,10);
    call addTarget(-1,10);
    return 0;
}


function nextStep(){
    call moveAllTargets(0,-1);
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

function moveDown(){
    return 0;
}

function moveUp(){
    return 0;
}