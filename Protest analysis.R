
#count of protests by date, color coded for violence and nonviolence
usprotests %>% group_by(event_date, event_violence) %>% summarize(count= n()) %>% ggplot(aes(event_date, count, color = event_violence)) + geom_point() + scale_x_date(date_labels = "%B", date_breaks = "1 month", expand = expansion(mult = c(.05, 0))) + scale_y_continuous(limits = c(0, 750)) + theme(axis.text.x = element_text(angle=90, hjust = 1)) + facet_grid(~ year(event_date), space="free_x", scales="free_x", switch="x") + labs(title="US Demonstrations", x=NULL, y= "Number of Incidents") + scale_color_manual(name = "", labels = c("Nonviolent", "Violent"), values = c("#009E73", "orange"))

#number of riots by date
usprotests %>% filter(event_type == "Riots") %>% group_by(event_date) %>% summarize(count= n()) %>% ggplot(aes(event_date, count)) + geom_point() + scale_x_date(date_labels = "%B", date_breaks = "1 month") + theme(axis.text.x = element_text(angle=90, hjust = 1))

#most common protest groups, in order
arrange((usprotests %>% group_by(assoc_actor_1) %>% summarize(count = n())), desc(count)) %>% print(n = 100)

BLM <- c("African American Group (United States)", "BLM: Black Lives Matter", "BLM: Black Lives Matter; NAACP: National Association for the Advancement of Colored People; African American Group (United States)", "Police Forces of the United States (2017-2021); BLM: Black Lives Matter", "BLM: Black Lives Matter; Police Forces of the United States (2017-2021)", "BLM: Black Lives Matter", "BLM: Black Lives Matter; Students (United States)", "BLM: Black Lives Matter; Civilians (United States); Journalists (United States)", "Students (United States); BLM: Black Lives Matter", "BLM: Black Lives Matter; Government of the United States (2017-2021)", "BLM: Black Lives Matter; LGBT (United States)", "BLM: Black Lives Matter; Christian Group (United States)", "BLM: Black Lives Matter; African American Group (United States)", "BLM: Black Lives Matter; Women (United States)", "BLM: Black Lives Matter; Lawyers (United States)", "BLM: Black Lives Matter; Protestant Christian Group (United States)", "BLM: Black Lives Matter; Health Workers (United States)", "BLM: Black Lives Matter; Students (United States); African American Group (United States)", "LGBT (United States); BLM: Black Lives Matter", "BLM: Black Lives Matter; Students (United States); Teachers (United States)", "BLM: Black Lives Matter; Labour Group (United States)", "BLM: Black Lives Matter; Government of the United States (2017-2021); Police Forces of the United States (2017-2021)", "BLM: Black Lives Matter; Health Workers (United States); SEIU: Service Employees International Union", "Government of the United States (2017-2021); BLM: Black Lives Matter", "BLM: Black Lives Matter; PSL: Party for Socialism and Liberation", "BLM: Black Lives Matter; Latinx Group (United States)", "BLM: Black Lives Matter; Teachers (United States)")
ProPolice <- c("Back the Blue; Pro-Police Group (United States)", "Pro-Police Group (United States)", "Back the Blue; Pro-Police Group (United States)", "Pro-Police Group (United States)", "Pro-Police Group (United States); Back the Blue", "Blue Lives Matter; Pro-Police Group (United States)", "Police Forces of the United States (2017-2021)")




"Proud Boys"
"Boogaloo Boys"
"Antifa (United States)"

usprotests %>% filter(farRight == TRUE | antifa == TRUE) %>% ggplot(aes(event_date, fill=farRight)) + geom_histogram(position="dodge") + scale_x_date(date_labels = "%B", date_breaks = "1 month") + theme(axis.text.x = element_text(angle=90, hjust = 1))

usprotests %>% filter(farRight == TRUE | antifa == TRUE) %>% filter(event_violence == "violent") %>% ggplot(aes(event_date, fill=farRight)) + geom_histogram(position="dodge") + scale_x_date(date_labels = "%B", date_breaks = "1 month") + theme(axis.text.x = element_text(angle=90, hjust = 1)) + scale_y_continuous(limits = c(0, 14))

usprotests %>% filter(farRight == TRUE | antifa == TRUE) %>% ggplot(aes(month(event_date), fill=farRight)) + geom_bar(position="dodge") + theme(axis.text.x = element_text(angle=90, hjust = 1)) + facet_grid(~ year(event_date), space="free_x", scales="free_x", switch="x") + scale_x_continuous(breaks = breaks_width(1))

#most complete graph of general Antifa and Far Right activity
usprotests %>% filter(farRight == TRUE | antifa == TRUE) %>% ggplot(aes(as.Date(paste0(as.character(as.yearmon(event_date)), " 15"), format = "%b %Y %d"), fill=farRight)) + geom_bar(position=position_dodge2(preserve = "single")) + theme(axis.text.x = element_text(angle=90, hjust = 1)) + scale_y_continuous(limits=c(0, 23), expand = c(0,0)) + scale_x_date(date_labels = "%B", date_breaks = "1 month", expand = expansion(mult = c(.05, 0))) + facet_grid(~ year(event_date), space="free_x", scales="free_x", switch="x") + labs(title="Demonstrations: Antifa and the Far Right", x=NULL, y= "Number of Incidents") + scale_fill_manual(name = "", labels = c("Antifa", "Far Right"), values = c("#0072B2", "#D55E00"))

#most complete graph of violence activity
usprotests %>% filter(event_violence == "violent") %>% filter(farRight == TRUE | antifa == TRUE) %>% ggplot(aes(as.Date(paste0(as.character(as.yearmon(event_date)), " 15"), format = "%b %Y %d"), fill=farRight)) + geom_bar(position=position_dodge2(preserve = "single")) + theme(axis.text.x = element_text(angle=90, hjust = 1)) + scale_y_continuous(limits=c(0, 23), expand = c(0,0)) + scale_x_date(date_labels = "%B", date_breaks = "1 month", expand = expansion(mult = c(.05, 0))) + facet_grid(~ year(event_date), space="free_x", scales="free_x", switch="x") + labs(title="Violent Acts: Antifa and the Far Right", x=NULL, y= "Number of Incidents") + scale_fill_manual(name = "", labels = c("Antifa", "Far Right"), values = c("#0072B2", "#D55E00"))