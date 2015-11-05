// -*- c -*-
const int pin_bow_onoff = 2;    // onoff key  ( yellow cable )
const int pin_bow_port  = 3;    // left       ( red cabel )
const int pin_bow_starb = 4;    // right      ( green cabel )
const int pin_bow_status = 5;   // onoff status ( orange )
// blue cable = gnd

const int pin_stern_onoff = 6;   // onoff key
const int pin_stern_port  = 7;   // left
const int pin_stern_starb = 8;   // right
const int pin_stern_status = 9;  // onoff status

#define BOW_ONOFF     0b00000001
#define BOW_PORT      0b00000010
#define BOW_STARB     0b00000100
#define STERN_ONOFF   0b00001000
#define STERN_PORT    0b00010000
#define STERN_STARB   0b00100000

#define STATUS_OFF     0
#define STATUS_ON      1
#define STATUS_STANDBY 2

int bow_status;
const int bow_on_delay    = 1000;     // 1000 ms constant before ON
const int bow_off_delay   = 5000;     // 5000 ms constant before OFF
unsigned long bow_time;
uint8_t bow_prev_v;

int stern_status;
const int stern_on_delay    = 1000;   // 1000 ms constant before ON
const int stern_off_delay   = 5000;   // 5000 ms constant before OFF
unsigned long stern_time;
uint8_t stern_prev_v;

uint8_t key_mask;

#define DELAY(t) delayMicroseconds((t))

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);

  pinMode(pin_bow_onoff,  OUTPUT);
  pinMode(pin_bow_port,   OUTPUT);
  pinMode(pin_bow_starb,  OUTPUT);
  pinMode(pin_bow_status, INPUT_PULLUP);

  pinMode(pin_stern_onoff,  OUTPUT);
  pinMode(pin_stern_port,   OUTPUT);
  pinMode(pin_stern_starb,  OUTPUT);
  pinMode(pin_stern_status, INPUT_PULLUP);

  digitalWrite(pin_bow_onoff, LOW);
  digitalWrite(pin_bow_port, LOW);
  digitalWrite(pin_bow_starb, LOW);

  digitalWrite(pin_stern_onoff, LOW);
  digitalWrite(pin_stern_port, LOW);
  digitalWrite(pin_stern_starb, LOW);
  
  bow_status = STATUS_OFF;
  bow_time = millis();
  
  stern_status = STATUS_OFF;
  stern_time = millis();
}

void write_pins(uint8_t mask, int level)
{
    if (mask & BOW_ONOFF) digitalWrite(pin_bow_onoff, level);
    if (mask & BOW_PORT)  digitalWrite(pin_bow_port, level);
    if (mask & BOW_STARB) digitalWrite(pin_bow_starb, level);

    if (mask & STERN_ONOFF) digitalWrite(pin_stern_onoff, level);
    if (mask & STERN_PORT) digitalWrite(pin_stern_port, level);
    if (mask & STERN_STARB) digitalWrite(pin_stern_starb, level);
}

void loop()
{
    unsigned long now;
    uint8_t  bow_s;
    uint8_t  stern_s;
    int bow_v;
    int stern_v;
    
    if (Serial.available() > 0) {
	key_mask = Serial.read();
	write_pins(~key_mask, LOW);  // first set pins to low
	write_pins(key_mask,  HIGH);  // then set pins high
    }
    now = millis();
    bow_v = !digitalRead(pin_bow_status);
    bow_s = bow_status;
    
    if (bow_v == bow_prev_v) {
      if ((bow_v == HIGH) && (key_mask & BOW_ONOFF) &&
          ((now - bow_time) >= bow_on_delay)) {
            bow_s = STATUS_ON;
            bow_time = now;
      }
     if ((bow_v == LOW) && !(key_mask & BOW_ONOFF) &&
          ((now - bow_time) >= bow_off_delay)) {
            bow_s = STATUS_OFF;
            bow_time = now;
      }
    }
    else {
       bow_s = STATUS_STANDBY;
       bow_time = now;
    }
    bow_prev_v = bow_v;

    stern_v = !digitalRead(pin_stern_status);
    stern_s = stern_status;
    
    if (stern_v == stern_prev_v) {
      if ((stern_v == HIGH) && (key_mask & BOW_ONOFF) &&
          ((now - stern_time) >= stern_on_delay)) {
            stern_s = STATUS_ON;
            stern_time = now;
      }
     if ((stern_v == LOW) && !(key_mask & BOW_ONOFF) &&
          ((now - stern_time) >= stern_off_delay)) {
            stern_s = STATUS_OFF;
            stern_time = now;
      }
    }
    else {
       stern_s = STATUS_STANDBY;
       stern_time = now;
    }
    stern_prev_v = stern_v;

    if ((bow_s != bow_status) || (stern_s != stern_status)) {
        Serial.write((stern_s << 2) | bow_s);
        bow_status = bow_s;
        stern_status = stern_s; 
    }
}

