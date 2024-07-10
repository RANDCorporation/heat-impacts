#### Script for checking package licenses

## Get packages from tool library
source("./tool_library.R", local = TRUE)

## Find attached packages
attached_packages <- (.packages())

## Function developed by Ricky Sanchez and Pedro Nascimento de Lima
ListLicenses = function(packages) {
  
  for (package_name in sort(packages)) {   
    
    first_order_dependencies <- packrat:::getPackageDependencies(packages, lib.loc = .libPaths()[1], fields = c("Depends", "Imports"))   
    nth_order_dependencies = packrat:::recursivePackageDependencies(package_name, lib.loc = .libPaths()[1], ignores = c(), fields = c("Depends", "Imports"))   
    nth_order_dependencies <- nth_order_dependencies[!nth_order_dependencies %in% first_order_dependencies]
    
    # Originally used, but only works for CRAN packages:
    license_info = packageDescription(package_name, fields="License")   
    print(glue::glue("{package_name}: {license_info}"))
    # print("1st Order Dependencies:")
    # for (dependency in unlist(first_order_dependencies)) {
    #   dependency_license_info = packageDescription(dependency, fields="License")
    #   print(glue::glue("\t{dependency}: {dependency_license_info}"))
    # }
    # 
    # print("2-Nth Order Dependencies:")
    # for (dependency in unlist(nth_order_dependencies)) {
    #   dependency_license_info = packageDescription(dependency, fields="License")
    #   print(glue::glue("\t{dependency}: {dependency_license_info}"))
    # }
  }
}

ListLicenses(attached_packages)