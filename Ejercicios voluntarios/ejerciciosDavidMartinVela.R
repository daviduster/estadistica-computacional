# David Alberto Martín Vela. Estadistica Computacional UGR 2020-2021
# Ejercicio pagina 28. Modifique la funcion para que devuelva tambien cuantos elementos habia y cuantos ha quitado. 
media = function(x=NA){
	tot_elem = length(x) 
	x <- x[!is.na(x)]
	tot_elem_quitados = tot_elem - length(x) 
	return (list(media = sum(x)/length(x), total_elementos = tot_elem, elementos_quitados = tot_elem_quitados))
}

# Ejercicio pagina 29. Escriba una funcion que calcule los coeficientes de asimetria y curtosis de Fisher. 
# Tambien se pueden utilizar directamente las funciones skewness y kurtosis del paquete library(moments)
skewness_kurtosis = function(x){
	m3 = mean((x - mean(x))^3)
	skew = m3/(sd(x)^3)
	
	m4 = mean((x- mean(x))^4)
	kurt = m4/(sd(x)^4)
	
	return (list(skeweness = skew, kurtosis = kurt))
}
