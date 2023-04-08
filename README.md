# How can I use this to prioritize my tasks?

```
~/work/pm master*
❯ ./run.sh
Enter a task description: create 1031exchange.ai
Enter the impact of the task (1-10): 5
Enter the urgency of the task (1-10): 8
Enter the effort estimate of the task (in hours): 100
New task created: Task {description = "create 1031exchange.ai", impact = 5, urgency = 8, hoursEstimate = 100, roi = 0.4}
Current queue:
Task {description = "create 1031exchange.ai", impact = 5, urgency = 8, hoursEstimate = 100, roi = 0.4}

~/work/pm master* 13s
❯ ./run.sh
Enter a task description: create a shopify store backup competitor
Enter the impact of the task (1-10): 7
Enter the urgency of the task (1-10): 8
Enter the effort estimate of the task (in hours): 100
New task created: Task {description = "create a shopify store backup competitor", impact = 7, urgency = 8, hoursEstimate = 100, roi = 0.56}
Current queue:
Task {description = "create a shopify store backup competitor", impact = 7, urgency = 8, hoursEstimate = 100, roi = 0.56}
Task {description = "create 1031exchange.ai", impact = 5, urgency = 8, hoursEstimate = 100, roi = 0.4}

~/work/pm master* 16s
❯
```
