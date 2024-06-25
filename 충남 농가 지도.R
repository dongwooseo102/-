library(geosphere)
library(ggmap)
library(shiny)
library(leaflet)
library(dplyr)
library(purrr)

register_google(key = "key")
addresses <- c("충남 홍성군 홍동면 충절로 371번길 107-45",
               "충남 홍성군 홍동면 충절로 344번길 62-55",
               "충남 홍성군 서부면 서부로 295-90",
               "충남 천안시 동남구 목천읍 소사3길 192",
               "충남 천안시 동남구 수신면 해정2길 34-27",
               "충남 천안시 동남구 병천면 봉항로 718")
# 주소를 UTF-8로 인코딩하고, geocode로 위도와 경도를 가져옵니다.
locations <- data.frame(
  name = c("해피팜스토리", "더힐링식물원팜","아가새농장홍성","아빠사랑팜","공방고운","봉황52농장"),
  address = addresses
)

locations <- locations %>%
  mutate(geocode_result = map(address, ~ geocode(enc2utf8(.))),
         lon = map_dbl(geocode_result, "lon"),
         lat = map_dbl(geocode_result, "lat"))

# 추가 정보
locations$reservation_status <- c("예약 현황: 10/30", "예약 현황: 20/30","예약 현황: 20/30","예약 현황: 20/30","예약 현황: 20/30","예약 현황: 20/30")
locations$description <- c("농업은 생명이라는 슬로건으로 농업의 중요성과 농촌의 다원적 가치를 전하고 싶은 농장입니다. 우리농산물의 중요성, 로컬푸드의 의미, 농업인으로서의 자긍심을 전달하고자 합니다. 함께하면 더 행복한 치유농장입니다.",
                           "	<온전한 휴식의 가치> - 비전 키워드 : 자연, 치유, 복합문화공간 - 농장 슬로건 : 더힐링으로 힐링, 가자! 식물을 심고 가꾸는 과정에서 느끼는 생명에 대한 배려, 돌봄과 함께 숲이 주는 긍정적 요소를 바탕으로 대상자의 심리적 안정을 추구하는 치유 체험형 농장입니다.",
                           "새조개와 대하축제로 유명한 남당항이 자리잡고 있는 충청남도 홍성군 서부면에 위치한 아가새농장홍성입니다. 연중 먹이체험과 동물교감이 가능한 아가새농장홍성은 16종의 앵무새 400여마리와 토끼, 기니피그, 팬더마우스, 고슴도치, 다람쥐 등 여러동물과 함께 살아가는 꿈꾸는 앵무새 농장입니다.",
                           "지속 발전 가능한 자연 순환 생태 축산과 동물복지의 중요성을 실천하는 농장입니다. 가축이 사람에게 주는 고마움과 중요성을 느끼고, 생명 존중의 가치를 아이들이 배울 수 있는 농촌교육농장입니다.",
                           "공방고운은 천연염색가 김경애가 자연과 함께 어울리며 창작 할 수 있도록 만든 공간입니다. 천연염색을 배우고 싶어하는 모든 사람들에게 언제든 열려있는 공간이기도 합니다. 천안시 수신면의 탁트인 한적한 자연 속에 위치한 공방고운은 자연에서 채취한 재료들을 이용해 아름다운 색을 표현할 수 있도록 체험 및 교육 프로그램을 병행하고 있습니다.",
                           "35년차 오이 농부의 진솔한 삶과 농업에 대한 철학을 아이들에게 전달 하고자 농촌교육농장을 시작 하였습니다. 벌써 4번째 품질인증을 신청하게 되어 감회가 새롭고 농업인으로써 교육 활동을 한다는 것에 큰 자부심을 갖고 있습니다.")
locations$image <- c("https://ifh.cc/g/gHD7Wf.jpg",
                     "https://ifh.cc/g/zgtzF9.jpg",
                     "https://ifh.cc/g/zrLxjh.jpg",
                     "https://ifh.cc/g/JD4pcF.jpg",
                     "https://ifh.cc/g/cwdHR0.jpg",
                     "https://ifh.cc/g/VcsbA5.jpg")


# Shiny 앱 UI 정의
ui <- fluidPage(
  titlePanel("충천남도 농촌교육농장 지도"),
  textInput("user_location", "현재 위치 (공주대학교 예산캠퍼스):", "공주대학교 예산캠퍼스"),
  actionButton("find_location", "위치 찾기"),
  leafletOutput("map")
)

# Shiny 앱 서버 정의
server <- function(input, output, session) {
  user_location <- reactiveVal(NULL)
  
  observeEvent(input$find_location, {
    geocode_result <- geocode(enc2utf8(input$user_location))
    user_location(c(lon = geocode_result$lon, lat = geocode_result$lat))
  })
  
  output$map <- renderLeaflet({
    req(user_location())
    
    distances <- distHaversine(
      matrix(c(locations$lon, locations$lat), ncol = 2),
      matrix(c(user_location()[1], user_location()[2]), ncol = 2)
    )
    
    locations <- locations %>%
      mutate(distance = round(distances / 1000, 2)) # km 단위로 변환
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(locations$lon), lat = mean(locations$lat), zoom = 10) %>%
      addMarkers(lng = user_location()[1], lat = user_location()[2], popup = "현재 위치") %>%
      addMarkers(data = locations, ~lon, ~lat, 
                 popup = ~paste0("<strong>", name, "</strong><br>",
                                 "<img src='", image, "' width='100' height='100'><br>",
                                 reservation_status, "<br>",
                                 description, "<br>",
                                 "거리: ", distance, " km"))
  })
}

shinyApp(ui, server)
