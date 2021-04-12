# David Alberto Martin Vela
# Ejercicio propuesto martes 6 de Abril de 2021, utilizar vectores
# auxiliares para la funcion dong3 (pagina 49 guion curso R)

dong3.v_improved = function(numero = 100){
	
	x = vector(mode = "numeric", length = numero)	
	y = vector(mode = "numeric", length = numero)	

	x[1] = 1
	y[1] = 1

	# Vectores auxiliares
	XXX = c(0, 0.85, 0.2, -0.15)
	XXY = c(0, 0.04, -0.26, 0.28)
	XYY = c(0, -0.04, 0.26, 0.26)
	YYY = c(0.25, 0.85, 0.22, 0.24)
	ctes = c(0, 1.6, 0, 1)

	for (i in 2:numero) {
		a = sample(4, 1, p = c(1, 85, 7, 7))
		x[i] = XXX[a] * x[i - 1] + XXY[a] * y[i - 1] 
		y[i] = XYY[a] * x[i - 1] + YYY[a] * y[i - 1] + ctes[a]
	}

	return(list(x = x[2:numero], y = y[2:numero]))

}

dong3_improved = function(numero = 100){

	x = vector(mode = "numeric", length = numero)
	y = vector(mode = "numeric", length = numero)

	x[1] = 1
	y[1] = 1

	# Vectores auxiliares
	XXX = c(0, 0.85, 0.2, -0.15)
	XXY = c(0, 0.04, -0.26, 0.28)
	XYY = c(0, -0.04, 0.26, 0.26)
	YYY = c(0.25, 0.85, 0.22, 0.24)
	ctes = c(0, 1.6, 0, 1)

	for(i in 2:numero){
		a = sample(100,1)
		index = 0

		if (a == 1) {
			index = 1
		} else {
			if(a <= 86){
				index = 2
			}
			else{
				if(a <= 93){
					index = 3
				}
				else{
					index = 4
				}
			}
		}		

		x[i] = XXX[index] * x[i - 1] + XXY[index] * y[i - 1] 
		y[i] = XYY[index] * x[i - 1] + YYY[index] * y[i - 1] + ctes[index]

	}

	return(list(x = x[2:numero], y = y[2:numero]))
}