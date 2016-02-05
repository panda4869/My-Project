
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 28 14:29:46 2015

@author: panminxiang
"""

#####
import pygame, sys, time, random    #@UnusedImport  
from pygame.locals import*        #@UnusedWildImport  
  
# size of window 
WIN_WIDTH  = 640  
WIN_HEIGHT = 480  
  
# game status  
GAME_INIT        = 0  
GAME_RUN         = 2  
GAME_GAMEOVER    = 3  
 
  
# ball status  
BALL_START_Y  = (WIN_HEIGHT//2)  
BALL_SIZE     = 4  
  
# panal status  
PADDLE_X  = (WIN_WIDTH/2 - 16)  
PADDLE_Y  = (WIN_HEIGHT - 32);  
PADDLE_WIDTH    = 100 
PADDLE_HEIGHT   = 8  
  
# brick status  
NUM_BLOCK_ROWS    = 6  
NUM_BLOCK_COLUMNS = 8  
BLOCK_WIDTH       = 64  
BLOCK_HEIGHT      = 16  
BLOCK_X    = 8  
BLOCK_Y    = 8  
BLOCK_X_GAP       = 80  
BLOCK_Y_GAP       = 32  
  
# color config    
BALL_COLOR       = (255, 255, 0)  
PADDLE_COLOR     = (0, 255, 0)  
BLOCK_COLOR      = (178, 34, 34)  
TEXT_COLOR       = (255, 255, 255)  
  
# game config  
TOTAL_LIFE       = 5  
FPS              = 25  
speed            = 0

#music
 
# create blocks 
def InitBlocks():  
      
    s=[] 
    for i in range(NUM_BLOCK_ROWS):  
        blocks = []
        for j in range(NUM_BLOCK_COLUMNS):    
           blocks.append(1)
        s.append(blocks)
    return s  

  
# PrintText  
def PrintText(text, font, surface, x, y):  
    text_obj = font.render(text, 1, TEXT_COLOR)  
    text_rect = text_obj.get_rect()  
    text_rect.topleft = (x, y)  
    surface.blit(text_obj, text_rect)  
  
# quit game 
def Terminate():  
    pygame.quit()  
    sys.exit()  
      
# waitfor player to press key 
def WaitForKey():  
    while True:  
        for event in pygame.event.get():  
            if event.type == QUIT:  
                Terminate()  
            if event.type == KEYDOWN:  
                if event.key == K_ESCAPE:  
                    Terminate()  
                return  
# check button status
def button_check(pos, x, y, x1, y1): 
    return pos[0] >= x and pos[0] < x + x1 and pos[1] >= y and pos[1] < y + y1

# add botton for manu
def make_button(surface,color,text_color,x,y,width,height,text):
    pygame.draw.rect(surface, (0,0,0),(x-1,y-1,width+2,height+2),1)
    pygame.draw.rect(surface, color,(x,y,width,height))

    myfont = pygame.font.SysFont('Arial Black', 32)
    label = myfont.render(text, 1, text_color)
    surface.blit(label, (x+280, y+30))
menu_items = ['Easy','Normal','Hard','Exit']
start_game=0
screen = pygame.display.set_mode((800, 600)) 
while True:   
    # initial   
    pygame.init()  
    mainClock = pygame.time.Clock()  
    pygame.mixer.music.load('/usr/local/lib/python2.7/site-packages/pygame/cotm_catacombs.mid') 
    #set background   
    background_image = pygame.image.load("/usr/local/lib/python2.7/site-packages/pygame/cast1.jpg").convert()
   
      
    # set surface config 
    windowSurface = pygame.display.set_mode((WIN_WIDTH, WIN_HEIGHT), 0, 32)  
    pygame.display.set_caption('Brick Breaker')
    for i in range(len(menu_items)):
        if(i!=1):
          make_button(windowSurface,(250, 128, 0) ,(0, 0, 225) ,0,(120*i),640,120,menu_items[i])
        else:
          make_button(windowSurface,(250, 128, 0) ,(0, 0, 225) ,-20,(120*i),660,120,menu_items[i])
    pygame.display.update()  
    #select difficulty and update the config
    #choose background image
    #choose music
    for event in pygame.event.get():
        if event.type == MOUSEBUTTONUP:
            
                for i in range(len(menu_items)):

                    if button_check(event.pos,0,(120*i),640,120):
                        if i == 0:
                            
                            NUM_BLOCK_ROWS    = 6  
                            NUM_BLOCK_COLUMNS = 8 
                            BLOCK_WIDTH = 64
                            BLOCK_HEIGHT      = 16    
                            BLOCK_X_GAP       = 80  
                            BLOCK_Y_GAP       = 32  
                            BALL_SIZE     = 10  
                            PADDLE_WIDTH    = 100
                            speed=4
                            start_game=1
                            difficult='Easy'
                            background_image = pygame.image.load("/usr/local/lib/python2.7/site-packages/pygame/cast1.jpg").convert()
                            pygame.mixer.music.load('/usr/local/lib/python2.7/site-packages/pygame/cotm_catacombs.mid') 
                        elif i == 1:
                            NUM_BLOCK_ROWS    = 7  
                            NUM_BLOCK_COLUMNS = 9 
                            BLOCK_WIDTH = 50
                            BLOCK_HEIGHT      = 12    
                            BLOCK_X_GAP       = 70 
                            BLOCK_Y_GAP       = 25  
                            BALL_SIZE     = 8  
                            PADDLE_WIDTH    = 80 
                            speed=8
                            start_game=1
                            difficult='Normal'
                            background_image = pygame.image.load("/usr/local/lib/python2.7/site-packages/pygame/cast2.jpg").convert()
                            pygame.mixer.music.load('/usr/local/lib/python2.7/site-packages/pygame/Vampire_Killer_1.mid') 
                        elif i == 2:
                            NUM_BLOCK_ROWS    = 10  
                            NUM_BLOCK_COLUMNS = 18 
                            BLOCK_WIDTH = 30
                            BLOCK_HEIGHT      = 12    
                            BLOCK_X_GAP       = 40 
                            BLOCK_Y_GAP       = 20  
                            BALL_SIZE     = 6  
                            PADDLE_WIDTH    = 60 
                            
                            speed=11
                            start_game=1
                            difficult='Hard'
                            background_image = pygame.image.load("/usr/local/lib/python2.7/site-packages/pygame/cast3.jpg").convert()
                            pygame.mixer.music.load('/usr/local/lib/python2.7/site-packages/pygame/Clockwork.mid') 
                        elif i == 3:
                            pygame.quit()
                            sys.exit()  
      
     # inital config for ball 
    ball_x  = 0  
    ball_y  = 0  
    ball_dx = 0  
    ball_dy = 0  
      
    # initial config for panal 
    paddle_move_left  = False  
    paddle_move_right = False  
      
    #   
    paddle  = {'re' :pygame.Rect(0, 0, PADDLE_WIDTH, PADDLE_HEIGHT),   
               'color': PADDLE_COLOR}  
      
    # initial game status  
    game_state  = GAME_INIT  
    blocks      = []  
    life_left   = TOTAL_LIFE  
    game_over   = False  
    blocks_hit  = 0  
    score       = 0  
    game_start_font = pygame.font.SysFont(None, 48)  
    game_over_font  = pygame.font.SysFont(None, 48)  
    text_font       = pygame.font.SysFont(None, 25)  
      
      
      
    # play background music  
    if start_game==1:
      pygame.mixer.music.play(-1, 0.0)
      
       
    while start_game==1:  
        
        # add listener  
        for event in pygame.event.get():  
            if event.type == QUIT:  
                Terminate()  
            if event.type == KEYDOWN:  
                if event.key == K_LEFT:  
                    paddle_move_left = True  
                if event.key == K_RIGHT:  
                    paddle_move_right = True  
                if event.key == K_ESCAPE:  
                    Terminate()  
            if event.type == KEYUP:  
                if event.key == K_LEFT:  
                    paddle_move_left = False  
                if event.key == K_RIGHT:  
                    paddle_move_right = False  
              
        # game flow             
        if game_state == GAME_INIT:  
            # initial key variables  
            ball_x  = random.randint(8, WIN_WIDTH-8)  
            ball_y  = BALL_START_Y  
            ball_dx = 0 
            ball_dy = speed  
              
            paddle['re'].left = PADDLE_X  
            paddle['re'].top  = PADDLE_Y  
              
            paddle_move_left  = False  
            paddle_move_right = False  
              
            life_left   = TOTAL_LIFE  
            game_over   = False  
            blocks_hit  = 0  
            score       = 0  
            level       = 1  
            blocks = InitBlocks()  
            game_state = GAME_RUN  
        elif game_state == GAME_RUN:  
              
              
            # movement of ball  
            ball_x += ball_dx;  
            ball_y += ball_dy;  
              
            if ball_x > (WIN_WIDTH-BALL_SIZE) or ball_x < BALL_SIZE:  
                ball_dx = -ball_dx  
                ball_x  += ball_dx;  
            elif ball_y < BALL_SIZE:  
                ball_dy = -ball_dy  
                ball_y  += ball_dy  
            elif ball_y > WIN_HEIGHT-BALL_SIZE:  
                if life_left == 0:  
                    game_state = GAME_GAMEOVER  
                else:  
                    life_left -= 1  
                    # restart the game  
                    ball_x  = paddle['re'].left + PADDLE_WIDTH // 2  
                    ball_y  = BALL_START_Y  
                    ball_dx = 0  
                    ball_dy = speed  
                  
            # check for the collision of paddle 
            if ball_y > WIN_HEIGHT // 2:  
                if (ball_x+BALL_SIZE >= paddle['re'].left and   
                    ball_x-BALL_SIZE <= paddle['re'].left+PADDLE_WIDTH and   
                    ball_y+BALL_SIZE >= paddle['re'].top and  
                    ball_y-BALL_SIZE <= paddle['re'].top+PADDLE_HEIGHT):  
                    ball_dy = - ball_dy  
                    ball_y += ball_dy  
                      
                    if paddle_move_left:  
                        ball_dx -= random.randint(0, 3)  
                    elif paddle_move_right:  
                        ball_dx += random.randint(0, 3)  
                    else:  
                        ball_dx += random.randint(-1, 2)  
                          
            # check for the collsion of bricks  
            cur_x = BLOCK_X  
            cur_y = BLOCK_Y  
            for row in range(NUM_BLOCK_ROWS):  
                cur_x = BLOCK_X  
                for col in range(NUM_BLOCK_COLUMNS):  
                    if blocks[row][col] != 0:  
                        if (ball_x+BALL_SIZE >= cur_x and   
                            ball_x-BALL_SIZE <= cur_x+BLOCK_WIDTH and   
                            ball_y+BALL_SIZE >= cur_y and  
                            ball_y-BALL_SIZE <= cur_y+BLOCK_HEIGHT):  
                            blocks[row][col] = 0  
                            blocks_hit += 1  
                            ball_dy = -ball_dy  
                            ball_dx += random.randint(-1, 2)  
                            score += 5 * (speed+abs(ball_dx))  
                    #move to the next block          
                    cur_x += BLOCK_X_GAP  
                cur_y += BLOCK_Y_GAP  
                  
            if blocks_hit == NUM_BLOCK_ROWS * NUM_BLOCK_COLUMNS:  
                  
                blocks_hit  = 0  
                #life left bonus
                score       += 500* life_left 
                game_state = GAME_GAMEOVER  
                 
            # movement of paddle  
            if paddle_move_left:  
                paddle['re'].left -= 16  
                if paddle['re'].left < 0:  
                    paddle['re'].left = 0  
            if paddle_move_right:  
                paddle['re'].left += 16  
                if paddle['re'].left > WIN_WIDTH-PADDLE_WIDTH:  
                    paddle['re'].left = WIN_WIDTH-PADDLE_WIDTH  
              
            # draw background image 
            screen.blit(background_image, [0, 0])
            #windowSurface.fill(BACKGROUND_COLOR)  
            # draw paddle  
            pygame.draw.rect(windowSurface, paddle['color'], paddle['re'])  
            #draw the ball  
            pygame.draw.circle(windowSurface, BALL_COLOR, (ball_x, ball_y),   
                               BALL_SIZE, 0)  
            # draw bricks  
            cur_x = BLOCK_X  
            cur_y = BLOCK_Y  
            for row in range(NUM_BLOCK_ROWS):  
                cur_x = BLOCK_X  
                for col in range(NUM_BLOCK_COLUMNS):  
                    if blocks[row][col] != 0:  
                        pygame.draw.rect(windowSurface, BLOCK_COLOR,   
                                         (cur_x, cur_y, BLOCK_WIDTH, BLOCK_HEIGHT))  
                    cur_x += BLOCK_X_GAP  
                cur_y += BLOCK_Y_GAP  
                  
            # add imformation board  
            message=' Difficulty:  '+difficult+'    Life: ' + str(life_left) + '    Score: ' + str(score)  
            PrintText(message, text_font, windowSurface, 8, (WIN_HEIGHT - 16)) 
            #go back to the manu
        elif game_state == GAME_GAMEOVER:
            start_game=0
            if blocks_hit == 0:
                PrintText('  Congraduations!', game_over_font, windowSurface,   
                         (WIN_WIDTH / 3)-35, (WIN_HEIGHT / 3))  
            else:
                PrintText('  Game Over', game_over_font, windowSurface,   
                         (WIN_WIDTH / 3)-10, (WIN_HEIGHT / 3))
            PrintText('Score: ' + str(score), game_over_font, windowSurface,   
                         (WIN_WIDTH / 3)+20, (WIN_HEIGHT / 3) + 100)  
            PrintText('Press any key to play again.', game_over_font, windowSurface,   
                         (WIN_WIDTH / 3)-80, (WIN_HEIGHT / 3) + 150)  
            pygame.display.update()  
               
              
            WaitForKey()  
            break  
          
          
        pygame.display.update()  
        mainClock.tick(FPS + speed) 