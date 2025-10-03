import serial
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys
import smtplib

email_sent = False

temp_state = 0

# Configure the serial port
ser = serial.Serial(
    port='COM3',
    baudrate=115200,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_TWO,
    bytesize=serial.EIGHTBITS
)

if not ser.isOpen():
    ser.open()

xsize = 500
soak_temp = 150
soak_time = 60
reflow_temp = 220
reflow_time = 45

def data_gen():
    global xsize
    global soak_temp
    global soak_time
    global reflow_temp
    global reflow_time

    t = -1
    email_state = 0
    while True:
        t += 1
        temp_bytes = ser.readline()  # Read the line from the serial port
        temp_str = temp_bytes.decode('utf-8').strip()  # Decode to string and strip \r\n
        if len(temp_str) > 4:
            try:
                values = temp_str.split()
                floats = [float(value) for value in values]  # Convert values to floats
                soak_temp = floats[0]
                reflow_temp = floats[2]
                print(reflow_temp)
                print(soak_temp)
                print("Read floats:", floats)
            except ValueError:
                print("Error: Could not convert values to floats")
        else:
            temp = float(temp_str)  # Convert the cleaned-up temperature value to float
        if temp > soak_temp and email_state == 0:
                email = "taranehtbaa@gmail.com"
                receiver_email = "schroeder.ryker@gmail.com"
                Subject = "State Report"
                message = "SOAKING"
                text = f"Subjct:{Subject}\n\n{message}"
                server = smtplib.SMTP("smtp.gmail.com", 587)
                server.starttls()
                server.login(email, "yben lyub oukm ghdn")
                server.sendmail(email, receiver_email, message)
                print("An email has been sent to " + receiver_email + "for soak")
                email_state = 1
                #email_sent = True 
        elif temp > reflow_temp and email_state == 1:
                email = "taranehtbaa@gmail.com"
                receiver_email = "schroeder.ryker@gmail.com"
                Subject = "State Report"
                message = "REFLOW"
                text = f"Subjct:{Subject}\n\n{message}"
                server = smtplib.SMTP("smtp.gmail.com", 587)
                server.starttls()
                server.login(email, "yben lyub oukm ghdn")
                server.sendmail(email, receiver_email, message)
                print("An email has been sent to " + receiver_email + "for reflow")
                email_state = 2
        elif temp < 60 and email_state == 2:
                email = "taranehtbaa@gmail.com"
                receiver_email = "schroeder.ryker@gmail.com"
                Subject = "State Report"
                message = "FINISH COOLING"
                text = f"Subjct:{Subject}\n\n{message}"
                server = smtplib.SMTP("smtp.gmail.com", 587)
                server.starttls()
                server.login(email, "yben lyub oukm ghdn")
                server.sendmail(email, receiver_email, message)
                print("An email has been sent to " + receiver_email + "for cooling finish")
                email_state = 0
        yield t, temp

def run(data):
    t, y = data
    global temp_state
    xdata.append(t)
    ydata.append(y)
    if t > xsize: 
        ax.set_xlim(t-xsize, t)
    line.set_data(xdata, ydata)
    
    if y > soak_temp and y <= reflow_temp and temp_state == 0:
        line.set_color('purple')
        temp_state = 1
        for txt in ax.texts:
            txt.set_visible(False)  # Clear previous messages to avoid clutter
        ax.text(t, y, 'Soaking', fontsize=8, color='black')
    elif y > reflow_temp and temp_state == 1:
        line.set_color('red')
        temp_state = 2
        for txt in ax.texts:
            txt.set_visible(False)  # Clear previous messages to avoid clutter
        ax.text(t, y, 'Reflow', fontsize=8, color='black')
    elif y < (reflow_temp - 10) and temp_state == 2:
        line.set_color('blue')  # Change line color back to blue (or any default color) if below 25°C
        for txt in ax.texts:
            txt.set_visible(False)  # Clear previous messages to avoid clutter
        ax.text(t, y, '', fontsize=8, color='black')
        #temp_state = 0
    return line,

def on_close_figure(event):
    ser.close()  
    sys.exit(0)

fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, = ax.plot([], [], lw=2)
ax.set_ylim(0, 250)  
ax.set_xlim(0, xsize)
ax.grid()
xdata, ydata = [], []
ax.set_xlabel('Time (s)', fontsize=12)  # X-axis label
ax.set_ylabel('Temperature (°C)', fontsize=12)  # Y-axis label


ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=100, repeat=False)
plt.show()