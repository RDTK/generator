(stp:serialize
 #.(CXML-STP:ELEMENT
    #| :PARENT of type ELEMENT |#
    :CHILDREN '(
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :LOCAL-NAME "healthy")

		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :LOCAL-NAME "unHealthy")


		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "[TASKS] "))
		   :LOCAL-NAME "pluginName")

		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :LOCAL-NAME "defaultEncoding")

		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "false"))
		   :LOCAL-NAME "canRunOnFailed")

		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "false"))
		   :LOCAL-NAME "useStableBuildAsReference")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
      ")
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "false"))
		   :LOCAL-NAME "useDeltaValues")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
      ")
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :ATTRIBUTES '(#.(CXML-STP:ATTRIBUTE
				    #| :PARENT of type ELEMENT |#
				    :VALUE "analysis-core@1.48"
				    :LOCAL-NAME "plugin"))
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "unstableTotalAll")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "unstableTotalHigh")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "unstableTotalNormal")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "unstableTotalLow")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "unstableNewAll")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "unstableNewHigh")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "unstableNewNormal")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "unstableNewLow")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "failedTotalAll")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "failedTotalHigh")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "failedTotalNormal")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "failedTotalLow")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "failedNewAll")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "failedNewHigh")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "failedNewNormal")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
        ")
			       #.(CXML-STP:ELEMENT
				  #| :PARENT of type ELEMENT |#
				  :LOCAL-NAME "failedNewLow")
			       #.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "
      "))
		   :LOCAL-NAME "thresholds")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
      ")
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "false"))
		   :LOCAL-NAME "shouldDetectModules")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
      ")
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "false"))
		   :LOCAL-NAME "dontComputeNew")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
      ")
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "false"))
		   :LOCAL-NAME "doNotResolveRelativePaths")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
      ")
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "FIXME,HACK"))
		   :LOCAL-NAME "high")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
      ")
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "TODO"))
		   :LOCAL-NAME "normal")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
      ")
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :LOCAL-NAME "low")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
      ")
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "true"))
		   :LOCAL-NAME "ignoreCase")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
      ")
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "**/*.lisp,**/*.asd"))
		   :LOCAL-NAME "pattern")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
      ")
		#.(CXML-STP:ELEMENT
		   #| :PARENT of type ELEMENT |#
		   :CHILDREN '(#.(CXML-STP:TEXT
				  #| :PARENT of type ELEMENT |#
				  :DATA "upstream/**/*.*, build/**/*.*"))
		   :LOCAL-NAME "excludePattern")
		#.(CXML-STP:TEXT
		   #| :PARENT of type ELEMENT |#
		   :DATA "
    "))
    :LOCAL-NAME )
 (cxml:make-character-stream-sink *standard-output*))
