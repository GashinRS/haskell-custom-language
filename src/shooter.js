width = (call getWidth()/2);
height = (call getHeight()/2);

function startGame(){
    bottom = (-1 * (call getHeight()/2));
    call setPlayerStart(0,bottom);
    call addTargets(-3,-2,-1,0,1,-4,-2,0,1,3,5,10,10,10,10,10,9,9,9,9,9,9);
    return 0;
}

function nextStep(){
    call moveAllTargets(0,-1);
    
    index = 0;
    bulletsAtBottom = 0;
    while (((index < call getWidth()) && (bulletsAtBottom == 0))){
        bulletsAtBottom = call targetAt(index, -height);
        index = (index + 1);
    }

    if((bulletsAtBottom == 1)){
        call setGameOver();
    }

    if((bulletsAtBottom == 0)){
        call removeOutOfBoundsTargets(width, height);
        call removeCollidingTargetsAndBullets();
        call moveAllBullets(0, 1);
        call removeOutOfBoundBullets(width, height);
        call removeCollidingTargetsAndBullets();
        if((call getTargetsAmount() == 0)){
            call setWon();
        }
    }
    return 0;
}

function pressSpace(){
    bulletX = call getPlayerX();
    bulletY = (call getPlayerY() + 1);
    call shoot(bulletX, bulletY);
    return 0;
}

function moveLeft(){
    if((call getPlayerX() > -width)){
        call updatePlayerPos(-1,0);
    }
    return 0;
}

function moveRight(){
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